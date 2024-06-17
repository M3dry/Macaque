{-# LANGUAGE DuplicateRecordFields #-}

module Static where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Parser.Types (ADT (..), ADTConstructor (..), File (..), FunctionSignature (..), TopLevel (..), Type (..), TypeIdentifier)
import Static.Types

convertADTs :: [ADT] -> Map TypeIdentifier Type
convertADTs = foldl (\m ADT {name, constructors} -> M.union (convertConstructors name constructors) m) M.empty

convertConstructors :: TypeIdentifier -> [ADTConstructor] -> Map TypeIdentifier Type
convertConstructors adtName =
  foldl
    ( \m constructor -> case constructor of
        ADTNormal name types -> M.insert name (foldl (\acc t -> TypeArrow t acc) (TypeSimple adtName) types) m
    )
    M.empty

-- createSymbols :: [FunctionSignature] -> [ADT] -> Symbols
-- createSymbols sigs adts =
--   Symbols
--     { constructors = convertADTs adts,
--       signatures = foldl (\m FunctionSignature {name, signature} -> M.insert name signature m) M.empty sigs
--     }

-- typeCheck :: File -> Maybe ()
-- typeCheck File {topLevel = topLevel} = do
--   let (adts, sigs, variants) =
--         foldl
--           ( \(adts', sigs', variants') tl -> case tl of
--               TopLvlADT adt -> (adt : adts', sigs', variants')
--               TopLvlFunSig sig -> (adts', sig : sigs', variants')
--               TopLvlFunVariant variant -> (adts', sigs', variant : variants')
--           )
--           ([], [], [])
--           topLevel
--   let symbols = createSymbols sigs adts
--   undefined
