{-# LANGUAGE OverloadedStrings #-}

module Parser.Util (Parser, lineComment, blockComment, scn, sc, lexeme, symbol, eol, indent, indentOnEOL, reserved, testParser, testParser') where

import Control.Applicative hiding (some)
import Control.Applicative.Combinators (some)
import Control.Monad.State (StateT (runStateT), get)
import Data.Functor (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof, hidden), Parsec, PosState (PosState), SourcePos (SourcePos), State (State, stateInput), errorBundlePretty, mkPos, runParser')
import Text.Megaparsec.Char (char, newline, space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser' c = StateT c (Parsec Void Text)

-- Int denotes the mininum indentation level in spaces that is allowed
type Parser = Parser' Int

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

eol :: Parser ()
eol = hidden (void (char '\n') <|> eof)

indent :: Int -> Parser ()
indent 0 = pure ()
indent level = (char ' ' <|> char '\t') *> indent (level - 1)

indentOnEOL :: Parser ()
indentOnEOL = do
    level <- get
    void $ optional (some newline *> (indent level *> sc *> indentOnEOL))

reserved :: [Text]
reserved = ["let", "in", "where", "if", "then", "else", "case", "of"]

testParser :: (Show a) => Parser a -> String -> IO ()
testParser p = testParser' p 0

testParser' :: (Show a) => Parser a -> Int -> String -> IO ()
testParser' p level i = do
    let i' = T.pack i
    let state = State i' 0 (PosState i' 1 (SourcePos "" (mkPos 1) (mkPos 1)) (mkPos 4) "") []
    let p' = runStateT p level
    let (state', parsed) = runParser' p' state
    putStrLn "Parsed:"
    case parsed of
        Right ok -> print ok
        Left err -> putStr $ errorBundlePretty err
    putStrLn "\nParsec State"
    print $ stateInput state'
