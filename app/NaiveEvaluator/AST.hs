{-# LANGUAGE DataKinds #-}

module NaiveEvaluator.AST (InterpreterError (..), IError, Interpreter, ValueInfo (..), showValue) where

import Data.Map qualified as M
import Data.Text qualified as T
import Effectful (Eff)
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader)
import Text.Megaparsec (SourcePos)

data InterpreterError
    = IEVariableNotDefined T.Text
    | IENotAFunction
    | IEBadPattern
    | IENoMatch
    | IEIfElseCondNotBool ValueInfo
    | IEBadArgumentToConstructor
    deriving (Show)

type IError = (SourcePos, InterpreterError)

type Interpreter a = Eff '[Reader (M.Map T.Text ValueInfo), Error IError] a

data ValueInfo
    = VUnit
    | VInt Int
    | VChar Char
    | VString T.Text
    | VConstructed T.Text [ValueInfo]
    | VTuple [ValueInfo]
    | VFunction (ValueInfo -> Either IError ValueInfo)

instance Show ValueInfo where
    show VUnit = "VUnit"
    show (VInt n) = "VInt " <> show n
    show (VChar ch) = "VChar " <> show ch
    show (VString txt) = "VSTring " <> show txt
    show (VConstructed name vs) = "VConstructed " <> show name <> show vs
    show (VTuple vs) = "VTuple " <> show vs
    show (VFunction _) = "VFunction"

showValue :: ValueInfo -> String
showValue VUnit = "()"
showValue (VInt n) = show n
showValue (VChar ch) = show ch
showValue (VString str) = show str
showValue (VConstructed name fields) = T.unpack name <> " " <> join " " (map showValue fields)
showValue (VTuple vs) = "(" <> join ", " (map showValue vs) <> ")"
showValue (VFunction _) = "function"

join :: String -> [String] -> String
join _ [] = ""
join _ [x] = x
join sep [x, y] = x <> sep <> y
join sep (x:y:xs) = x <> sep <> y <> sep <> join sep xs
