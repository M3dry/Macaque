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
