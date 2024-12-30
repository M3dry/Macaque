{-# LANGUAGE TypeFamilies #-}

module Parser.AST (Parsing) where

import AST
import Text.Megaparsec (SourcePos)

data Parsing

type instance TypeArrow Parsing = SourcePos
type instance TypeTuple Parsing = SourcePos
type instance TypeSimple Parsing = SourcePos
type instance TypeHole Parsing = SourcePos
type instance TypeUnit Parsing = SourcePos

type instance LitNumber Parsing = SourcePos
type instance LitString Parsing = SourcePos
type instance LitChar Parsing = SourcePos

type instance PatVariable Parsing = SourcePos
type instance PatCapture Parsing = SourcePos
type instance PatConstructor Parsing = SourcePos
type instance PatLiteral Parsing = SourcePos
type instance PatIgnore Parsing = SourcePos

type instance ExprVar Parsing = SourcePos
type instance ExprTypeConstructor Parsing = SourcePos
type instance ExprApplied Parsing = SourcePos
type instance ExprTyped Parsing = SourcePos
type instance ExprLet Parsing = SourcePos
type instance ExprIfElse Parsing = SourcePos
type instance ExprCase Parsing = SourcePos
type instance ExprLambda Parsing = SourcePos
type instance ExprLiteral Parsing = SourcePos
