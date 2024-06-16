{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Static.TypeChecker where

import Control.Applicative ((<|>))
import Control.Monad.Reader (MonadTrans (lift), ask)
import Control.Monad.State (MonadState (get, put))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Parser.Types (Expression (..), Identifier, Literal (..), Pattern (..), Type (..), TypeIdentifier (TypeIdentifier))
import Static.Types

typeCheckExpr :: Expression -> TypeChecker Type
typeCheckExpr (ExprVar iden) = do
    Symbols{signatures} <- ask
    idens <- get
    lift . lift $ (M.lookup iden signatures <|> M.lookup iden idens)
typeCheckExpr (ExprTypeConstructor tIden) = do
    Symbols{constructors} <- ask
    lift . lift $ M.lookup tIden constructors
typeCheckExpr (ExprApplied fExpr argExpr) = do
    fExprT <- typeCheckExpr fExpr
    argExprT <- typeCheckExpr argExpr
    t' <- lift . lift $ eqTypes fExprT (TypeArrow TypeHole argExprT)
    return t'
typeCheckExpr (ExprTyped expr t) = do
    exprT <- typeCheckExpr expr
    t' <- lift . lift $ eqTypes exprT t
    return t'
typeCheckExpr (ExprLet iden idenExpr expr) = do
    idenExprT <- typeCheckExpr idenExpr
    get >>= put . M.insert iden idenExprT
    exprT <- typeCheckExpr expr
    get >>= put . M.delete iden
    return exprT
typeCheckExpr (ExprIfElse condExpr trueExpr falseExpr) = do
    condExprT <- typeCheckExpr condExpr
    condExprT' <- lift . lift $ eqTypes condExprT (TypeSimple $ TypeIdentifier "Bool")
    trueExprT <- typeCheckExpr trueExpr
    falseExprT <- typeCheckExpr falseExpr
    t' <- lift . lift $ eqTypes trueExprT falseExprT
    return t'
typeCheckExpr (ExprCase expr pats) = do
    undefined
typeCheckExpr (ExprLambda paramPats expr) = do
    params <- mapM (typeCheckPattern) paramPats
    undefined
typeCheckExpr (ExprLiteral (LitNumber _ _)) = return $ TypeSimple $ TypeIdentifier "Int"
typeCheckExpr (ExprLiteral (LitString _)) = return $ TypeSimple $ TypeIdentifier "String"
typeCheckExpr (ExprLiteral (LitChar _)) = return $ TypeSimple $ TypeIdentifier "Char"

typeCheckPattern :: Pattern safe -> TypeChecker (Type, Map Identifier Type)
typeCheckPattern (PatVariable iden) = return (TypeHole, M.singleton iden TypeHole)
typeCheckPattern (PatCapture iden pat) = do
    (patT, idens) <- typeCheckPattern pat
    return (patT, M.insert iden patT idens)
typeCheckPattern PatIgnore = return (TypeHole, M.empty)
typeCheckPattern (PatConstructor tIden pats) = do
    Symbols{constructors} <- ask
    constructorParams <- lift . lift . fmap unfoldTypeArrow $ M.lookup tIden constructors
    case (constructorParams) of
        (retT : paramTs) | length paramTs == length pats -> do
            idens <-
                foldl
                    ( \acc (paramT, pat) -> do
                        idens <- acc
                        (patT, patIdens) <- typeCheckPattern pat
                        _ <- lift . lift $ eqTypes patT paramT
                        return $ M.union idens patIdens
                    )
                    (return M.empty)
                    $ zip (reverse paramTs) pats
            return (retT, idens)
        [retT] | null pats -> return (retT, M.empty)
        _ -> fail ""
typeCheckPattern (PatLiteral (LitNumber _ _)) = return (TypeSimple $ TypeIdentifier "Int", M.empty)
typeCheckPattern (PatLiteral (LitString _)) = return (TypeSimple $ TypeIdentifier "String", M.empty)
typeCheckPattern (PatLiteral (LitChar _)) = return (TypeSimple $ TypeIdentifier "Char", M.empty)

unfoldTypeArrow :: Type -> [Type]
unfoldTypeArrow = unfoldTypeArrow'
  where
    unfoldTypeArrow' (TypeArrow t retT) = retT : unfoldTypeArrow t
    unfoldTypeArrow' t = [t]

eqTypes :: Type -> Type -> Maybe (Type)
eqTypes TypeHole TypeHole = return TypeHole
eqTypes t TypeHole = return t
eqTypes TypeHole t = return t
eqTypes (TypeArrow t1 r1) (TypeArrow t2 r2) = do
    t <- eqTypes t1 t2
    r <- eqTypes r1 r2
    return $ TypeArrow t r
eqTypes (TypeTuple ts1) (TypeTuple ts2) = do
    ts <-
        foldr
            ( \(t1, t2) acc -> do
                acc' <- acc
                t <- eqTypes t1 t2
                return $ t : acc'
            )
            (return [])
            $ zip ts1 ts2
    return $ TypeTuple ts
eqTypes (TypeSimple tIden1) (TypeSimple tIden2)
    | tIden1 == tIden2 = return $ TypeSimple tIden1
    | otherwise = Nothing
eqTypes _ _ = Nothing
