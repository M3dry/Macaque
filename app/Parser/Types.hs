{-# LANGUAGE DuplicateRecordFields #-}

module Parser.Types (Identifier (..), TypeIdentifier (..), Type (..), ADT (..), ADTVariant (..), Function (..)) where

import Data.Text (Text)

newtype Identifier = Identifier Text
    deriving (Show)

newtype TypeIdentifier = TypeIdentifier Text
    deriving (Show)

data Type
    = TypeArrow Type Type
    | TypeTuple [Type]
    | TypeSimple TypeIdentifier
    | TypeInt
    | TypeChar
    deriving (Show)

data ADT = ADT
    { name :: TypeIdentifier
    , body :: [ADTVariant]
    }
    deriving (Show)

data ADTVariant
    = ADTUnit TypeIdentifier
    | ADTComplex TypeIdentifier [Type]
    deriving (Show)

data Function = Function
    { name :: Identifier
    , signature :: Type
    }
    deriving (Show)
