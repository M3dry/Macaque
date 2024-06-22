{-# LANGUAGE DataKinds #-}

module Static.Types (TaggedType, Type'(..), FunctionKey, Symbols (..), TypeChecker, runTypeChecker) where

import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (StateT, evalStateT)
import Data.Map.Strict (Map)
import Parser.Types (Identifier, Pattern, TypeIdentifier, Type)

data Type' f
    = TypeArrow' (f (Type' f)) (f (Type' f))
    | TypeTuple' [f (Type' f)]
    | TypeSimple' TypeIdentifier
    | TypeHole'

type TaggedType = ([Identifier], Type' ((,) [Identifier]))

instance Show t => Show (Type' ((,) t)) where
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

runTypeChecker :: Symbols -> Map Identifier TaggedType -> TypeChecker ret -> Maybe ret
runTypeChecker symbols idens tc = evalStateT (runReaderT tc symbols) idens