{-# LANGUAGE DuplicateRecordFields #-}

module Parser.Types (Identifier (..), TypeIdentifier (..), Type (..), ADT (..), ADTConstructor (..), Function (..)) where

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

data Function = Function
    { name :: Identifier
    , signature :: Type
    }
    deriving (Show)
