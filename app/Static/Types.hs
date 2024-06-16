{-# LANGUAGE DataKinds #-}

module Static.Types (FunctionKey, Symbols (..), TypeChecker) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Data.Map.Strict (Map)
import Parser.Types (Identifier, Pattern, Type, TypeIdentifier)

type FunctionKey = (Identifier, [Pattern 'False])

data Symbols = Symbols
    { signatures :: Map Identifier Type
    , constructors :: Map TypeIdentifier Type
    }

type TypeChecker ret =
    ReaderT
        Symbols -- (Symbols, Map Identifier Type)
        ( StateT
            (Map Identifier Type)
            Maybe
        )
        ret -- TODO: error handling
        -- Maybe
        -- ret
