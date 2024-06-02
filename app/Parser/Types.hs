{-# LANGUAGE DuplicateRecordFields #-}

module Parser.Types (
    Identifier (..),
    TypeIdentifier (..),
    Type (..),
    ADT (..),
    ADTConstructor (..),
    Pattern (..),
    Expression (..),
    FunctionSignature (..),
    FunctionVariants (..),
    FunctionVariant (..),
)
where

import Data.Text (Text)

newtype Identifier = Identifier Text
    deriving (Show)

newtype TypeIdentifier = TypeIdentifier Text
    deriving (Show)

data Type
    = TypeArrow Type Type
    | TypeTuple [Type]
    | TypeSimple TypeIdentifier
    deriving (Show)

data ADT = ADT
    { name :: TypeIdentifier
    , constructors :: [ADTConstructor]
    }
    deriving (Show)

data ADTConstructor = ADTNormal TypeIdentifier [Type]
    deriving (Show)

data Pattern
    = PatVariable Identifier
    | PatConstructor TypeIdentifier [Pattern]
    | PatCapture Identifier Pattern
    deriving (Show)

data Expression
    = ExprVar Identifier
    | ExprTypeConstructor TypeIdentifier
    | ExprApplied Expression Expression
    | ExprTyped Expression Type
    | ExprLet Identifier Expression Expression
    | ExprIfElse Expression Expression Expression
    | ExprCase Expression [(Pattern, Expression)]
    deriving (Show)

data FunctionSignature = FunctionSignature
    { name :: Identifier
    , signature :: Type
    }
    deriving (Show)

data FunctionVariants = FunctionVariants
    { name :: Identifier
    , variants :: [FunctionVariant]
    }

data FunctionVariant = FunctionVariant
    { patterns :: [Pattern]
    , expression :: Expression
    }
