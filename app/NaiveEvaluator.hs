{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module NaiveEvaluator (run, run', extractGADT, makeFun) where

import AST
import NaiveEvaluator.AST
import Parser.AST (Parsing)

import Control.Monad (zipWithM)
import Data.Functor.Foldable (cata)
import Data.Map qualified as M
import Data.Text qualified as T
import Effectful (Eff, runPureEff, (:>))
import Effectful.Error.Static (Error, runError, throwError, tryError)
import Effectful.Reader.Static (ask, asks, local, runReader)
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
                , VFunction $ \v ->
                    let !_ = unsafePerformIO $ putStrLn $ showValue v
                     in return VUnit
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
evaluate (ExprTypeConstructor _ (TypeIdentifier typeIden)) = do
    x <- asks (M.lookup typeIden)
    throwOnNothing x IEVariableNotDefined
evaluate (ExprApplied _ fExpr argExpr) = do
    fValue <- evaluate fExpr
    f <- case fValue of
        VLambda f -> return f
        VFunction f -> return $ \v -> case f v of
            Right v' -> return v'
            Left err -> throwError err
        _ -> throwError IENotAFunction
    argValue <- evaluate argExpr
    f argValue
evaluate (ExprTyped _ expr _) = evaluate expr
evaluate (ExprLet _ idens bodyExpr) = do
    m <-
        foldMap
            ( \(Identifier iden, idenExpr) -> do
                value <- evaluate idenExpr
                return $ M.singleton iden value
            )
            idens
    local (M.union m) $ evaluate bodyExpr
evaluate (ExprIfElse _ condExpr trueExpr falseExpr) = do
    condValue <- evaluate condExpr
    case condValue of
        VConstructed "False" [] -> evaluate falseExpr
        VConstructed "True" [] -> evaluate trueExpr
        v -> throwError $ IEIfElseCondNotBool v
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
evaluate (ExprLambda _ [pat] expr) =
    return $ VLambda $ \v -> do
        m <- patValues v pat
        local (M.union m) $ evaluate expr
evaluate (ExprLambda tag (pat : pats) expr) = return $ VLambda $ \v -> do
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
litValue (LitString _ txt) = VString txt
litValue (LitChar _ ch) = VChar ch
litValue (LitUnit _) = VUnit

patValues :: ValueInfo -> Pattern Parsing -> Interpreter (M.Map T.Text ValueInfo)
patValues value (PatVariable _ (Identifier iden)) = do
    return $ M.singleton iden value
patValues value (PatCapture _ (Identifier iden) pat) = do
    m <- patValues value pat
    return $ M.insert iden value m
patValues (VConstructed name vs) (PatConstructor _ (TypeIdentifier typeIden) pats)
    | name == typeIden = foldr M.union M.empty <$> zipWithM patValues vs pats
    | otherwise = throwError IEBadPattern
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

extractGADT :: AST.GADT Parsing -> M.Map T.Text ValueInfo
extractGADT (AST.GADT _ (TypeIdentifier typeName) constructors) = foldMap extractConstructor constructors
  where
    extractConstructor :: AST.Constructor Parsing -> M.Map T.Text ValueInfo
    extractConstructor (AST.Constructor _ (TypeIdentifier name) t) = M.singleton name $ fromType name t []

    fromType :: T.Text -> AST.Type Parsing -> [ValueInfo] -> ValueInfo
    fromType name (TypeArrow _ arg ret) passedDown = VFunction $ \v -> do
        arg' <- case (arg, v) of
            (TypeUnit _, VUnit) -> return v
            (TypeSimple _ (TypeIdentifier "Int"), VInt _) -> return v
            (TypeSimple _ (TypeIdentifier "Char"), VChar _) -> return v
            (TypeSimple _ (TypeIdentifier "String"), VString _) -> return v
            (TypeSimple _ _, VConstructed _ _) -> return v -- BUG: assumes the costructor constructs the right type
            (TypeTuple _ ts, VTuple vs) | length ts == length vs -> return v
            (TypeArrow{}, VLambda _) -> return v -- INFO: can't determine the type of a value function, this is what a type checker is for
            (TypeArrow{}, VFunction _) -> return v -- INFO: can't determine the type of a value function, this is what a type checker is for
            (TypeHole _, _) -> return v -- NOTE: IG a hole is any type???
            _ -> Left IEBadArgumentToConstructor
        return $ fromType name ret (arg' : passedDown)
    fromType name (TypeSimple _ (TypeIdentifier typeName')) vs
        | typeName' == typeName = VConstructed name $ reverse vs
    fromType _ _ _ = error "How did this get through the parser lol"

makeFun :: AST.Function Parsing -> Maybe (T.Text, Interpreter ValueInfo)
makeFun (AST.Function p (Identifier name) signature variants) =
    let sigLength =
            cata
                ( \case
                    TypeArrowF _ _ n -> n + 1
                    _ -> 0
                )
                signature
     in if all (\(FunctionVariant _ pats _) -> length pats <= sigLength) variants
            then
                Just
                    ( name
                    , do
                        context <- ask
                        case fun signature context [] of
                            Right v -> return v
                            Left err -> throwError err
                    )
            else Nothing
  where
    fun :: AST.Type Parsing -> M.Map T.Text ValueInfo -> [ValueInfo] -> Either InterpreterError ValueInfo
    fun (TypeArrow _ _ t) context vs = return $ VFunction $ \v -> fun t context (v : vs)
    fun _ context vs =
        let match_var = name <> T.pack "#match_var"
            case_expr =
                ExprCase
                    p
                    (ExprVar p (Identifier match_var))
                    $ map (\(FunctionVariant p' pats expr) -> (PatTuple p' pats, expr)) variants
         in case runPureEff $ runError $ runReader (M.insert match_var (VTuple $ reverse vs) context) $ evaluate case_expr of
                Right v -> Right v
                Left (_, err) -> Left err
