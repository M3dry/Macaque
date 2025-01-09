module Static (symbolsInScopeFunction, symbolsInScopeExpr) where

import AST
import Control.Arrow (first)
import Data.Functor.Foldable (cata)
import Data.Set qualified as S
import Data.Text qualified as T
import Parser.AST (Parsing)

symbolsInScopeFunction :: Function Parsing -> S.Set T.Text
symbolsInScopeFunction (Function _ (Identifier name) _ variants) = S.delete name $ S.unions $ map symbolsInScopeFunctionVariant variants

symbolsInScopeFunctionVariant :: FunctionVariant Parsing -> S.Set T.Text
symbolsInScopeFunctionVariant (FunctionVariant _ pats expr) = S.difference (symbolsInScopeExpr expr) $ S.unions $ map symbolsFromPattern pats

--- returns the set of all needed global symbols
symbolsInScopeExpr :: Expression Parsing -> S.Set T.Text
symbolsInScopeExpr = cata alg
  where
    alg (ExprVarF _ (Identifier iden)) = S.singleton iden
    alg (ExprTypeConstructorF _ (TypeIdentifier typeIden)) = S.singleton typeIden
    alg (ExprAppliedF _ f arg) = S.union f arg
    alg (ExprTypedF _ expr _) = expr
    alg (ExprLetF _ idens body) =
        let idens' = S.fromList $ map ((\(Identifier iden) -> iden) . fst) idens
         in S.difference (S.unions $ body : map snd idens) idens'
    alg (ExprIfElseF _ cond trueBranch falseBranch) = S.unions [cond, trueBranch, falseBranch]
    alg (ExprCaseF _ match branches) = S.union match $ foldMap (uncurry S.difference . first symbolsFromPattern) branches
    alg (ExprLambdaF _ pats body) = S.difference body $ foldMap symbolsFromPattern pats
    alg (ExprLiteralF _ _) = S.empty
    alg (ExprTupleF _ exprs) = S.unions exprs

symbolsFromPattern :: Pattern Parsing -> S.Set T.Text
symbolsFromPattern = cata alg
  where
    alg (PatVariableF _ (Identifier iden)) = S.singleton iden
    alg (PatCaptureF _ (Identifier iden) pat) = S.insert iden pat
    alg (PatConstructorF _ (TypeIdentifier typeIden) pats) = S.insert typeIden $ S.unions pats
    alg (PatLiteralF _ _) = S.empty
    alg (PatTupleF _ pats) = S.unions pats
    alg (PatIgnoreF _) = S.empty
