{-# LANGUAGE DataKinds #-}

module Static.Types (TypedExpression (..), FunctionKey, Symbols (..), TypeChecker) where

import Control.Monad.Reader (ReaderT)
import Data.Map.Strict (Map)
import Parser.Types (Identifier, Literal, Pattern, Type, TypeIdentifier)

data TypedExpression
  = TypedExprVar (Identifier, Type)
  | TypedExprTypeConstructor (TypeIdentifier, Type)
  | TypedExprApplied (TypedExpression, Type) (TypedExpression, Type)
  | TypedExprLet Identifier (TypedExpression, Type) (TypedExpression, Type)
  | TypedExprIfElse TypedExpression (TypedExpression, Type) (TypedExpression, Type)
  | TypedExprCase (TypedExpression, Type) [(Pattern 'False, (TypedExpression, Type))]
  | TypedExprLambda [Pattern 'True] (TypedExpression, Type)
  | TypedExprLiteral Literal
  deriving (Show)

type FunctionKey = (Identifier, [Pattern 'False])

data Symbols = Symbols
  { signatures :: Map Identifier Type,
    constructors :: Map TypeIdentifier Type
  }

type TypeChecker ret =
  ReaderT
    (Symbols, Map Identifier Type)
    Maybe
    ret -- TODO: error handling
