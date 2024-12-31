{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module NaiveEvaluator (run) where

import AST
import NaiveEvaluator.AST
import Parser.AST (Parsing)

import Control.Monad (zipWithM)
import Data.Map qualified as M
import Data.Text qualified as T
import Effectful (Eff, runPureEff, (:>))
import Effectful.Error.Static (Error, runError, throwError, tryError)
import Effectful.Reader.Static (asks, local, runReader)
import GHC.IO.Unsafe (unsafePerformIO)

throwOnNothing :: (Error InterpreterError :> es) => Maybe a -> InterpreterError -> Eff es a
throwOnNothing Nothing = throwError
throwOnNothing (Just a) = return . const a

run :: Expression Parsing -> ValueInfo
run =
    run' $
        M.fromList
            [
                ( T.pack "print"
                , VFunction $ \v -> do
                    _ <- return $ unsafePerformIO $ putStrLn $ showValue v
                    return VUnit
                )
            ]

run' :: M.Map T.Text ValueInfo -> Expression Parsing -> ValueInfo
run' context expr = case runPureEff $ runError $ runReader context $ evaluate expr of
    Right value -> value
    Left (callstack, err) -> error $ show err ++ "\n\n" ++ show callstack

evaluate :: Expression Parsing -> Interpreter ValueInfo
evaluate (ExprVar _ (Identifier iden)) = do
    x <- asks (M.lookup iden)
    throwOnNothing x IEVariableNotDefined
-- Should never be called as the parser doesn't allow for user defined GADTs
evaluate (ExprTypeConstructor _ (TypeIdentifier typeIden)) = do
    x <- asks (M.lookup typeIden)
    throwOnNothing x IEVariableNotDefined
evaluate (ExprApplied _ fExpr argExpr) = do
    fValue <- evaluate fExpr
    f <- case fValue of
        VFunction f -> return f
        _ -> throwError IENotAFunction
    argValue <- evaluate argExpr
    f argValue
evaluate (ExprTyped _ expr _) = evaluate expr
evaluate (ExprLet _ (Identifier iden) idenExpr bodyExpr) = do
    idenValue <- evaluate idenExpr
    local (M.insert iden idenValue) $ evaluate bodyExpr
evaluate (ExprIfElse _ condExpr trueExpr falseExpr) = do
    condValue <- evaluate condExpr
    case condValue of
        VUnit -> evaluate falseExpr
        _ -> evaluate trueExpr
evaluate (ExprCase _ matchExpr branches) = do
    matchValue <- evaluate matchExpr
    branches' <-
        mapM
            ( \(pat, branch) -> do
                patVars <- tryError @InterpreterError $ patValues matchValue pat
                return (patVars, branch)
            )
            branches
    firstMatch branches'
  where
    firstMatch [] = throwError IENoMatch
    firstMatch ((Left _, _) : xs) = firstMatch xs
    firstMatch ((Right patVars, branch) : _) = local (M.union patVars) $ evaluate branch
evaluate (ExprLambda _ [] _) = do
    error "Can't have lambda with zero args, How did this get throught the parser lol"
evaluate (ExprLambda _ [pat] expr) = return $ VFunction $ \v -> do
    m <- patValues v pat
    local (M.union m) $ evaluate expr
evaluate (ExprLambda tag (pat : pats) expr) = return $ VFunction $ \v -> do
    m <- patValues v pat
    local (M.union m) $ evaluate (ExprLambda tag pats expr)
evaluate (ExprLiteral _ lit) = return $ litValue lit
evaluate (ExprTuple _ exprs) = do
    values <- mapM evaluate exprs
    return $ VTuple values

litValue :: Literal Parsing -> ValueInfo
litValue (LitNumber _ sign txt) =
    VInt $
        (if sign == Positive then id else negate) $
            read @Int $
                T.unpack txt
litValue (LitString _ txt) = VString $ T.unpack txt
litValue (LitChar _ ch) = VChar ch
litValue (LitUnit _) = VUnit

patValues :: ValueInfo -> Pattern Parsing -> Interpreter (M.Map T.Text ValueInfo)
patValues value (PatVariable _ (Identifier iden)) = do
    return $ M.singleton iden value
patValues value (PatCapture _ (Identifier iden) pat) = do
    m <- patValues value pat
    return $ M.insert iden value m
patValues value (PatConstructor _ (TypeIdentifier typeIden) pats) = do
    error "can't happen"
patValues value (PatLiteral _ lit) = do
    let litV = litValue lit
    case (litV, value) of
        (VInt n1, VInt n2) | n1 == n2 -> return M.empty
        (VChar ch1, VChar ch2) | ch1 == ch2 -> return M.empty
        (VString str1, VString str2) | str1 == str2 -> return M.empty
        (VUnit, VUnit) -> return M.empty
        _ -> throwError IEBadPattern
patValues (VTuple values) (PatTuple _ pats) = foldr M.union M.empty <$> zipWithM patValues values pats
patValues _ (PatIgnore _) = return M.empty
patValues _ _ = throwError IEBadPattern
