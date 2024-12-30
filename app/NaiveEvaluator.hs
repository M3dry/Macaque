module NaiveEvaluator () where

import AST
import NaiveEvaluator.AST
import Parser.AST

import Control.Arrow (first)
import Data.Functor.Foldable (Recursive (cata))

annotate :: Expression Parsing -> Expression Typed
annotate = cata alg
  where
    alg :: ExpressionF Parsing (Expression Typed) -> Expression Typed
    alg (ExprVarF pos iden) = ExprVar (pos, TAny) iden
    alg (ExprTypeConstructorF pos typeIden) = ExprTypeConstructor (pos, TAny) typeIden
    alg (ExprAppliedF pos expr1 expr2) = ExprApplied (pos, TAny) expr1 expr2
    alg (ExprTypedF pos expr t) = ExprTyped (pos, TAny) expr (convert t) -- Can't trust the user, so won't assume the type of expr
    alg (ExprLetF pos iden idenExpr expr) = ExprLet (pos, snd $ extractTag expr) iden idenExpr expr
    alg (ExprIfElseF pos condExpr trueExpr falseExpr) = ExprIfElse (pos, TAny) condExpr trueExpr falseExpr
    alg (ExprCaseF pos matchExpr pats) = ExprCase (pos, TAny) matchExpr (map (first patF) pats)
    alg (ExprLambdaF pos pats expr) = ExprLambda (pos, TAny) (map patF pats) expr
    alg (ExprLiteralF pos lit) = let (t, lit') = litF lit in ExprLiteral (pos, t) lit'

    litF :: Literal Parsing -> (TypeInfo, Literal Typed)
    litF (LitNumber tag sign num) = (TSimple "Int", LitNumber tag sign num)
    litF (LitString tag str) = (TSimple "String", LitString tag str)
    litF (LitChar tag ch) = (TSimple "Char", LitChar tag ch)

    patF :: Pattern Parsing -> Pattern Typed
    patF = cata patAlg

    patAlg :: PatternF Parsing (Pattern Typed) -> Pattern Typed
    patAlg (PatVariableF tag iden) = PatVariable (tag, TAny) iden
    patAlg (PatCaptureF tag iden pat) = PatCapture (tag, snd $ extractTag pat) iden pat
    patAlg (PatConstructorF tag typeIden pats) = PatConstructor (tag, TAny) typeIden pats
    patAlg (PatLiteralF tag lit) = let (t, lit') = litF lit in PatLiteral (tag, t) lit'
    patAlg (PatIgnoreF tag) = PatIgnore (tag, TAny)
