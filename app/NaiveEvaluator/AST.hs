{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module NaiveEvaluator.AST (Typed, TypeInfo (..), TypeInfoF (..), Annotation) where

import AST
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Parser.AST (Parsing)
import Text.Megaparsec (SourcePos)

data Typed

data TypeInfo
    = TAny
    | TUnit
    | TSimple String
    | TTuple [TypeInfo]
    | TArrow TypeInfo TypeInfo

type Annotation = (SourcePos, TypeInfo)

makeBaseFunctor ''TypeInfo

type instance TypeArrow Typed = TypeArrow Parsing
type instance TypeTuple Typed = TypeTuple Parsing
type instance TypeSimple Typed = TypeSimple Parsing
type instance TypeHole Typed = TypeHole Parsing
type instance TypeUnit Typed = TypeUnit Parsing

type instance LitNumber Typed = LitNumber Parsing
type instance LitString Typed = LitString Parsing
type instance LitChar Typed = LitString Parsing

type instance PatVariable Typed = Annotation
type instance PatCapture Typed = Annotation
type instance PatConstructor Typed = Annotation
type instance PatLiteral Typed = Annotation
type instance PatIgnore Typed = Annotation

type instance ExprVar Typed = Annotation
type instance ExprTypeConstructor Typed = Annotation
type instance ExprApplied Typed = Annotation
type instance ExprTyped Typed = Annotation
type instance ExprLet Typed = Annotation
type instance ExprIfElse Typed = Annotation
type instance ExprCase Typed = Annotation
type instance ExprLambda Typed = Annotation
type instance ExprLiteral Typed = Annotation
