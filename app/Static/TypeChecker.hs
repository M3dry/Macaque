{-# LANGUAGE PatternSynonyms #-}

module Static.TypeChecker (annotate) where

import AST (Expression (..), ExpressionF (..), Extract (extractTag), Identifier (Identifier), Type (..), TypeIdentifier (TypeIdentifier), convert)
import Data.Functor.Foldable (cata)
import Data.Map qualified as M
import Data.Text qualified as T
import Parser.AST (Parsing)
import Static.AST (TypeOrAny, Typed, wrap, pattern TAny, pattern TArrow, pattern TSimple, pattern TTuple, pattern TUnit)

annotate :: M.Map T.Text (Type ()) -> Expression Parsing -> Expression Typed
annotate global = cata alg

typeCheckExpr :: Expression -> TypeChecker TaggedType
typeCheckExpr (ExprVar iden) = do
    Symbols{signatures} <- ask
    idens <- get
    lift . lift $ (M.lookup iden idens <|> (fmap typeToTagged $ M.lookup iden signatures))
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
>>>>>>> Stashed changes
  where
    alg :: ExpressionF Parsing (Expression Typed) -> Expression Typed
    alg (ExprVarF p i@(Identifier iden)) = ExprVar (maybe TAny wrap $ iden `M.lookup` global, p) i
    alg (ExprTypeConstructorF p ti@(TypeIdentifier typeIden)) = ExprTypeConstructor (maybe TAny wrap $ typeIden `M.lookup` global, p) ti
    alg (ExprAppliedF p f arg) =
        let (fT, _) = extractTag f
            (argT, _) = extractTag arg
         in ExprApplied (TArrow fT argT, p) f arg
    alg (ExprTypedF p expr t) =
        let (exprT, _) = extractTag expr
         in case (exprT, t) of
                (TAny, _) -> ExprTyped (wrap t, p) expr (convert t)
                _ | isSame t exprT -> ExprTyped (wrap t, p) expr (convert t)
                _ -> error ""
    alg (ExprLetF p idens body) = undefined
    alg (ExprIfElseF p cond trueBranch falseBranch) = undefined
    alg (ExprCaseF p match branches) = undefined
    alg (ExprLambdaF p pats body) = undefined
    alg (ExprLiteralF p _) = undefined
    alg (ExprTupleF p exprs) = undefined

isSame :: Type tag -> TypeOrAny -> Bool
isSame (TypeArrow _ f arg) (TArrow f' arg') = isSame f f' && isSame arg arg'
isSame (TypeTuple _ ts) (TTuple ts') = all (uncurry isSame) $ zip ts ts'
isSame (TypeSimple _ iden) (TSimple iden') = iden == iden'
isSame (TypeUnit _) TUnit = True
isSame _ _ = False

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
