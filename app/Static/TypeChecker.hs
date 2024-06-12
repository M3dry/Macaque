{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Static.TypeChecker where

import Control.Applicative ((<|>))
import Control.Monad.Reader (MonadTrans (lift), ask, local)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Parser.Types (Expression (..), Identifier, Literal (..), Pattern (..), Type (..), TypeIdentifier (TypeIdentifier))
import Static.Types

typeCheckExpr :: Expression -> TypeChecker (TypedExpression, Type)
typeCheckExpr (ExprVar iden) = do
  (Symbols {signatures}, locals) <- ask
  t <- (lift $ M.lookup iden locals) <|> (lift $ M.lookup iden signatures)
  return (TypedExprVar (iden, t), t)
typeCheckExpr (ExprTypeConstructor tIden) = do
  (Symbols {constructors}, _) <- ask
  t <- lift $ M.lookup tIden constructors
  return (TypedExprTypeConstructor (tIden, t), t)
typeCheckExpr (ExprApplied expr1 expr2) = do
  (tExpr1, expr1T@(TypeArrow tOfExpr2 resultT)) <- typeCheckExpr expr1
  (tExpr2, expr2T) <- typeCheckExpr expr2
  if tOfExpr2 `eqTypes` expr2T
    then return (TypedExprApplied (tExpr1, expr1T) (tExpr2, expr2T), resultT)
    else fail ""
typeCheckExpr (ExprTyped expr t) = do
  (tExpr, t') <- typeCheckExpr expr
  if t `eqTypes` t'
    then return (tExpr, t)
    else fail ""
typeCheckExpr (ExprLet iden idenExpr expr) = do
  (tIdenExpr, idenT) <- typeCheckExpr idenExpr
  (tExpr, t) <-
    local
      (\(global, locals) -> (global, M.insert iden idenT locals))
      $ typeCheckExpr expr
  return $ (TypedExprLet iden (tIdenExpr, idenT) (tExpr, t), t)
typeCheckExpr (ExprIfElse condExpr trueExpr falseExpr) = do
  (tCondExpr, TypeSimple (TypeIdentifier "Bool")) <- typeCheckExpr condExpr
  (tTrueExpr, trueExprT) <- typeCheckExpr trueExpr
  (tFalseExpr, falseExprT) <- typeCheckExpr falseExpr
  if trueExprT `eqTypes` falseExprT
    then return (TypedExprIfElse tCondExpr (tTrueExpr, trueExprT) (tFalseExpr, trueExprT), trueExprT)
    else fail ""
typeCheckExpr (ExprCase expr branches) = do
  (tExpr, exprT) <- typeCheckExpr expr
  branches' <-
    mapM
      ( \(pat, branchExpr) -> do
          (patT, binded, holeTs) <- typeCheckPattern pat
          binded' <- lift $ case patT of
            TypeHole -> Just $ foldl (\binded' holeT -> M.insert holeT exprT binded') binded holeTs
            _ ->
              if patT `eqTypes` exprT
                then Just binded
                else Nothing
          (tBranchExpr, branchExprT) <- local (\(global, locals) -> (global, M.union binded' locals)) $ typeCheckExpr branchExpr
          return (pat, tBranchExpr, branchExprT)
      )
      branches
  (branches'', t) <-
    lift $
      foldl
        ( \acc (pat, tBranchExpr, branchExprT) -> do
            (acc', t) <- acc
            if branchExprT `eqTypes` t
              then return ((pat, (tBranchExpr, t)) : acc', t)
              else fail ""
        )
        (Just ([], TypeHole))
        branches'
  return (TypedExprCase (tExpr, exprT) branches'', t)
-- TODO: Fuck me
typeCheckExpr (ExprLambda pats expr) = do
  undefined
typeCheckExpr (ExprLiteral literal) = do
  return $
    ( TypedExprLiteral literal,
      TypeSimple . TypeIdentifier $ case literal of
        LitNumber _ _ -> "Int"
        LitString _ -> "String"
        LitChar _ -> "Char"
    )

typeCheckPattern :: Pattern safe -> TypeChecker (Type, Map Identifier Type, [Identifier])
typeCheckPattern (PatVariable iden) = return (TypeHole, M.empty, [iden])
typeCheckPattern (PatCapture iden pat) = do
  (t, binded, holed) <- typeCheckPattern pat
  case holed of
    [] -> return (t, M.insert iden t binded, [])
    xs -> return (t, binded, iden : xs)
typeCheckPattern PatIgnore = return (TypeHole, M.empty, [])
typeCheckPattern (PatConstructor tIden pats) = do
  (Symbols {constructors}, _) <- ask
  t <- lift $ M.lookup tIden constructors
  pats' <- mapM typeCheckPattern pats
  case (t, pats') of
    (TypeArrow argTs retT, pats') -> do
      let argTs' = unfoldTypeArrow argTs
      binded <-
        lift
          $ foldl
            ( \acc (constructorArgT, (patT, binded, holeTs)) ->
                case patT of
                  TypeHole ->
                    Just $
                      M.union binded $
                        M.union (fromMaybe M.empty acc) $
                          foldl (\m holeT -> M.insert holeT constructorArgT m) M.empty holeTs
                  _ ->
                    if constructorArgT `eqTypes` patT
                      then Just binded
                      else Nothing
            )
            (Just M.empty)
          $ zip argTs' pats'
      return (retT, binded, [])
    (retT@(TypeSimple tIden'), []) | tIden' == tIden -> return (retT, M.empty, [])
    _ -> fail ""
typeCheckPattern (PatLiteral literal) =
  return
    ( TypeSimple . TypeIdentifier $ case literal of
        LitNumber _ _ -> "Int"
        LitString _ -> "String"
        LitChar _ -> "Char",
      M.empty,
      []
    )

unfoldTypeArrow :: Type -> [Type]
unfoldTypeArrow = reverse . unfoldTypeArrow'
  where
    unfoldTypeArrow' (TypeArrow t retT) = retT : unfoldTypeArrow t
    unfoldTypeArrow' t = [t]

eqTypes :: Type -> Type -> Bool
eqTypes (TypeArrow t1 t2) (TypeArrow t1' t2') = t1 `eqTypes` t1' && t2 `eqTypes` t2'
eqTypes (TypeTuple ts) (TypeTuple ts') = all (uncurry eqTypes) $ zip ts ts'
eqTypes (TypeSimple tIden) (TypeSimple tIden') = tIden == tIden'
eqTypes _ _ = False
