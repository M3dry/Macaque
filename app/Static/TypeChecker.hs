{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Static.TypeChecker where

import Control.Applicative ((<|>))
import Control.Monad.Reader (MonadTrans (lift), ask)
import Control.Monad.State (MonadState (get, put), modify)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Parser.Types (Expression (..), Identifier, Literal (..), Pattern (..), Type (..), TypeIdentifier (TypeIdentifier))
import Static.Types



typeCheckExpr :: Expression -> TypeChecker TaggedType
typeCheckExpr (ExprVar iden) = return ([iden], TypeHole')
typeCheckExpr (ExprTypeConstructor tIden) = do
    Symbols{constructors} <- ask
    constructorT <- lift . lift $ M.lookup tIden constructors
    return $ typeToTagged constructorT
typeCheckExpr (ExprApplied fExpr paramExpr) = do
    (fTagged, TypeArrow' paramT retT) <- typeCheckExpr fExpr
    paramExprT <- typeCheckExpr paramExpr
    paramT' <- eqTypes paramExprT paramT
    _ <- eqTypes (fTagged, TypeArrow' paramT retT) (fTagged, TypeArrow' paramT' retT)
    return retT
typeCheckExpr (ExprTyped expr t) = do
    let t' = typeToTagged t
    x <- typeCheckExpr expr
    eqTypes x t'
typeCheckExpr (ExprLet iden idenExpr expr) = do
    (taggedIdenExpr, idenExprT) <- typeCheckExpr idenExpr
    idens <- get
    put $ M.insert iden (iden : taggedIdenExpr, idenExprT) idens
    exprT <- typeCheckExpr expr
    put idens
    return exprT
typeCheckExpr (ExprIfElse condExpr trueExpr falseExpr) = do
    condT <- typeCheckExpr condExpr
    _ <- eqTypes condT (typeToTagged $ TypeSimple (TypeIdentifier "Bool"))
    trueT <- typeCheckExpr trueExpr
    falseT <- typeCheckExpr falseExpr
    eqTypes trueT falseT
typeCheckExpr (ExprCase expr pats) = do
    undefined
typeCheckExpr (ExprLambda paramPats expr) = do
    (taggedTs, idens) <-  foldl (\(ts, ms) (t, m) -> (t:ts, M.union m ms)) ([], M.empty) <$> mapM typeCheckPattern paramPats
    exprT <- typeCheckExpr expr
    modify (M.union idens)
    return $ foldl (\acc t -> ([], TypeArrow' t acc)) exprT taggedTs
typeCheckExpr (ExprLiteral (LitNumber _ _)) = return $ typeToTagged $ TypeSimple (TypeIdentifier "Int")
typeCheckExpr (ExprLiteral (LitString _)) = return $ typeToTagged $ TypeSimple (TypeIdentifier "String")
typeCheckExpr (ExprLiteral (LitChar _)) = return $ typeToTagged $ TypeSimple (TypeIdentifier "Char")

typeCheckPattern :: Pattern safe -> TypeChecker (TaggedType, Map Identifier TaggedType)
typeCheckPattern (PatVariable iden) = return (([iden], TypeHole'), M.singleton iden ([iden], TypeHole'))
typeCheckPattern _ = undefined

unfoldTypeArrow :: Type -> [Type]
unfoldTypeArrow = unfoldTypeArrow'
  where
    unfoldTypeArrow' (TypeArrow t retT) = retT : unfoldTypeArrow t
    unfoldTypeArrow' t = [t]

eqTypes :: TaggedType -> TaggedType -> TypeChecker TaggedType
eqTypes (idens1, t1) (idens2, t2) = do
    t <- eqTypes' t1 t2
    idens <- get
    put $ foldl (\m iden -> do M.update (\(taggedIdens, _) -> return (taggedIdens, t)) iden m) idens $ idens1 ++ idens2
    return (idens1 ++ idens2, t)
  where
    eqTypes' :: Type' ((,) [Identifier]) -> Type' ((,) [Identifier]) -> TypeChecker (Type' ((,) [Identifier]))
    eqTypes' (TypeArrow' taggedT1 taggedRetT1)
             (TypeArrow' taggedT2 taggedRetT2) = do
        taggedT <- eqTypes taggedT1 taggedT2
        taggedRetT <- eqTypes taggedRetT1 taggedRetT2
        return $ TypeArrow' taggedT taggedRetT
    eqTypes' (TypeTuple' taggedTs1) (TypeTuple' taggedTs2) = do
        undefined
    eqTypes' t@(TypeSimple' tIden1) (TypeSimple' tIden2) = if tIden1 == tIden2
        then return t
        else fail ""
    eqTypes' t TypeHole' = return t
    eqTypes' TypeHole' t = return t
    eqTypes' _ _ = fail ""
  
typeToTagged :: Type -> TaggedType
typeToTagged = ([],) . typeToTagged'
  where
    typeToTagged' :: Type -> Type' ((,) [Identifier])
    typeToTagged' (TypeArrow t retT) = TypeArrow' ([], typeToTagged' t) ([], typeToTagged' retT)
    typeToTagged' (TypeTuple ts) = TypeTuple' $ map typeToTagged ts
    typeToTagged' (TypeSimple tIden) = TypeSimple' tIden
    typeToTagged' TypeHole = TypeHole'