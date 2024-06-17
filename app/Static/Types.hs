{-# LANGUAGE DataKinds #-}

module Static.Types (TaggedType, Type'(..), FunctionKey, Symbols (..), TypeChecker) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Data.Map.Strict (Map)
import Parser.Types (Identifier, Pattern, TypeIdentifier, Type)

data Type' f
    = TypeArrow' (f (Type' f)) (f (Type' f))
    | TypeTuple' [f (Type' f)]
    | TypeSimple' TypeIdentifier
    | TypeHole'

type TaggedType = ([Identifier], Type' ((,) [Identifier]))

instance Show (Type' ((,) Identifier)) where
    show (TypeArrow' paramT retT) = "TypeArrow' " ++ show paramT ++ " " ++ show retT
    show (TypeTuple' ts) = "TypeTuple' " ++ show ts
    show (TypeSimple' tIden) = "TypeSimple' (" ++ show tIden ++ ")"
    show TypeHole' = "TypeHole'"

type FunctionKey = (Identifier, [Pattern 'False])

data Symbols = Symbols
    { signatures :: Map Identifier Type
    , constructors :: Map TypeIdentifier Type
    }

type TypeChecker ret =
    ReaderT
        Symbols -- (Symbols, Map Identifier Type)
        ( StateT
            (Map Identifier TaggedType)
            Maybe
        )
        ret -- TODO: error handling
        -- Maybe
        -- ret
