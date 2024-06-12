{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Parser.Types (
    Identifier (..),
    TypeIdentifier (..),
    Type (..),
    ADT (..),
    Literal (..),
    ADTConstructor (..),
    Pattern (..),
    Expression (..),
    FunctionSignature (..),
    FunctionVariant (..),
    TopLevel (..),
    File (..),
)
where

import Data.Text (Text)

newtype Identifier = Identifier Text
    deriving (Show, Eq, Ord)

newtype TypeIdentifier = TypeIdentifier Text
    deriving (Show, Eq, Ord)

data Type
    = TypeArrow Type Type
    | TypeTuple [Type]
    | TypeSimple TypeIdentifier
    | TypeHole
    deriving (Show)

data ADT = ADT
    { name :: TypeIdentifier
    , constructors :: [ADTConstructor]
    }
    deriving (Show)

data ADTConstructor = ADTNormal TypeIdentifier [Type]
    deriving (Show)

data Literal
    = LitNumber Bool Text -- False = -, True = +
    | LitString Text
    | LitChar Char
    deriving (Show)

data Pattern (safe :: Bool) where
    -- safe
    PatVariable :: Identifier -> Pattern safe
    PatCapture :: Identifier -> Pattern safe -> Pattern safe
    PatIgnore :: Pattern safe
    -- either, checked later
    PatConstructor :: TypeIdentifier -> [Pattern safe] -> Pattern safe
    -- unsafe
    PatLiteral :: Literal -> Pattern 'False

deriving instance Show (Pattern safe)

data Expression
    = ExprVar Identifier
    | ExprTypeConstructor TypeIdentifier
    | ExprApplied Expression Expression
    | ExprTyped Expression Type
    | ExprLet Identifier Expression Expression
    | ExprIfElse Expression Expression Expression
    | ExprCase Expression [(Pattern 'False, Expression)]
    | ExprLambda [Pattern 'True] Expression
    | ExprLiteral Literal
    deriving (Show)

data FunctionSignature = FunctionSignature
    { name :: Identifier
    , signature :: Type
    }
    deriving (Show)

data FunctionVariant = FunctionVariant
    { name :: Identifier
    , patterns :: [Pattern 'False]
    , expression :: Expression
    }
    deriving (Show)

data TopLevel
    = TopLvlADT ADT
    | TopLvlFunSig FunctionSignature
    | TopLvlFunVariant FunctionVariant
    deriving (Show)

data File = File
    { topLevel :: [TopLevel]
    }
    deriving (Show)
