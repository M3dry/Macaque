{-# LANGUAGE DataKinds #-}

module NaiveEvaluator.AST (InterpreterError (..), Interpreter, ValueInfo (..), showValue) where

import Data.Map qualified as M
import Data.Text qualified as T
import Effectful (Eff)
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader)

data InterpreterError
    = IEVariableNotDefined
    | IENotAFunction
    | IEBadPattern
    | IENoMatch
    deriving (Show)

type Interpreter a = Eff '[Reader (M.Map T.Text ValueInfo), Error InterpreterError] a

data ValueInfo
    = VUnit
    | VInt Int
    | VChar Char
    | VString String
    | VTuple [ValueInfo]
    | VFunction (ValueInfo -> Interpreter ValueInfo)

showValue :: ValueInfo -> String
showValue VUnit = "()"
showValue (VInt n) = show n
showValue (VChar ch) = show ch
showValue (VString str) = show str
showValue (VTuple vs) = "(" <> join ", " (map showValue vs) <> ")"
showValue (VFunction _) = "function"

join :: String -> [String] -> String
join _ [] = ""
join _ [x] = x
join sep [x, y] = x <> sep <> y
join sep (x:y:xs) = x <> sep <> y <> sep <> join sep xs
