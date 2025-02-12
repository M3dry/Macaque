{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module Static.AST (
    Typed,
    TypeOrAny,
    wrap,
    pattern TAny,
    pattern TArrow,
    pattern TTuple,
    pattern TSimple,
    pattern TUnit,
    TypeChecker,
) where

import AST
import Data.Map qualified as M
import Data.Text qualified as T
import Effectful (Eff)
import Effectful.Reader.Static (Reader)
import Parser.AST (Parsing)
import Text.Megaparsec (SourcePos)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Functor.Foldable (cata)

data Typed

data TypeOrAny
    = TAny
    | TType (TypeF () TypeOrAny)

makeBaseFunctor ''TypeOrAny

wrap :: Type tag -> TypeOrAny
wrap = cata alg
  where
    alg (TypeArrowF _ f arg) = TArrow f arg
    alg (TypeTupleF _ ts) = TTuple ts
    alg (TypeSimpleF _ iden) = TSimple iden
    alg (TypeUnitF _) = TUnit

pattern TArrow :: TypeOrAny -> TypeOrAny -> TypeOrAny
pattern TArrow f arg = TType (TypeArrowF () f arg)

pattern TTuple :: [TypeOrAny] -> TypeOrAny
pattern TTuple ts = TType (TypeTupleF () ts)

pattern TSimple :: TypeIdentifier -> TypeOrAny
pattern TSimple typeIden = TType (TypeSimpleF () typeIden)

pattern TUnit :: TypeOrAny
pattern TUnit = TType (TypeUnitF ())

type TypeChecker a = Eff '[Reader (M.Map T.Text TypeOrAny)] a

type instance TypeArrow () = ()
type instance TypeTuple () = ()
type instance TypeSimple () = ()
type instance TypeUnit () = ()

type instance TypeArrow Typed = TypeArrow Parsing
type instance TypeTuple Typed = TypeTuple Parsing
type instance TypeSimple Typed = TypeSimple Parsing
type instance TypeUnit Typed = TypeUnit Parsing

type instance LitNumber Typed = (TypeOrAny, SourcePos)
type instance LitString Typed = (TypeOrAny, SourcePos)
type instance LitChar Typed = (TypeOrAny, SourcePos)
type instance LitUnit Typed = (TypeOrAny, SourcePos)

type instance PatVariable Typed = (TypeOrAny, SourcePos)
type instance PatCapture Typed = (TypeOrAny, SourcePos)
type instance PatConstructor Typed = (TypeOrAny, SourcePos)
type instance PatLiteral Typed = (TypeOrAny, SourcePos)
type instance PatTuple Typed = (TypeOrAny, SourcePos)
type instance PatIgnore Typed = (TypeOrAny, SourcePos)

type instance ExprVar Typed = (TypeOrAny, SourcePos)
type instance ExprTypeConstructor Typed = (TypeOrAny, SourcePos)
type instance ExprApplied Typed = (TypeOrAny, SourcePos)
type instance ExprTyped Typed = (TypeOrAny, SourcePos)
type instance ExprLet Typed = (TypeOrAny, SourcePos)
type instance ExprIfElse Typed = (TypeOrAny, SourcePos)
type instance ExprCase Typed = (TypeOrAny, SourcePos)
type instance ExprLambda Typed = (TypeOrAny, SourcePos)
type instance ExprLiteral Typed = (TypeOrAny, SourcePos)
type instance ExprTuple Typed = (TypeOrAny, SourcePos)

type instance GADTtag Typed = (TypeOrAny, SourcePos)
type instance Constructortag Typed = (TypeOrAny, SourcePos)

type instance Functiontag Typed = (TypeOrAny, SourcePos)
type instance FunctionVarianttag Typed = (TypeOrAny, SourcePos)
