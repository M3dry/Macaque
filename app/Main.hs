module Main where

import Parser (testExprP )
import Parser.Util (testParser)
import NaiveEvaluator (run)
import NaiveEvaluator.AST (showValue)

main :: IO ()
main = do
    expr <- testParser testExprP
    putStrLn $ showValue $ run expr
