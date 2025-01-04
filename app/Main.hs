{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow (second)
import Control.Monad (guard)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Effectful (runPureEff)
import Effectful.Error.Static (runError)
import Effectful.Reader.Static (runReader)
import NaiveEvaluator (extractGADT, makeFun, run, run')
import NaiveEvaluator.AST (showValue)
import Parser
import Parser.Util (testParser)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
    (gadts, funs) <- testParser testP
    let gadts' = foldMap extractGADT gadts
    let funs' = mapMaybe makeFun funs
    guard (length funs' == length funs)
    let global_scope =
            M.union gadts' $
                M.fromList $
                    map
                        ( second
                            ( ( \case
                                    Right v -> v
                                    Left err -> error $ show err
                              )
                                . runPureEff
                                . runError
                                . runReader global_scope
                            )
                        )
                        funs'
    case M.lookup (T.pack "main") global_scope of
        Just m -> putStrLn $ showValue m
        Nothing -> putStrLn "No main function"
