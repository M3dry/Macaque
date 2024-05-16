{-# LANGUAGE OverloadedStrings #-}

module Parser.Util (Parser, lineComment, blockComment, scn, sc, lexeme, symbol, testParser) where

import Control.Applicative hiding (some)
import Control.Applicative.Combinators (some)
import Data.Functor (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, PosState (PosState), SourcePos (SourcePos), State (State, stateInput), errorBundlePretty, mkPos, runParser')
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"

scn :: Parser ()
scn = L.space space1 lineComment blockComment

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

testParser :: (Show a) => Parser a -> String -> IO ()
testParser p i = do
    let i' = T.pack i
    let state = State i' 0 (PosState i' 1 (SourcePos "" (mkPos 1) (mkPos 1)) (mkPos 4) "") []
    let (state', parsed) = runParser' p state
    putStrLn "Parsed:"
    case parsed of
        Right ok -> print ok
        Left err -> putStr $ errorBundlePretty err
    putStrLn "\nState"
    print $ stateInput state'