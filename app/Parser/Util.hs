{-# LANGUAGE OverloadedStrings #-}

module Parser.Util (Parser, whiteSpace, whiteSpace', lexeme, lexeme', withLineFold, withLineFold', withIndent, inParens, testParser, testParser') where

import Control.Applicative hiding (some)
import Control.Applicative.Combinators (some, between)
import Control.Monad.State (StateT (runStateT), get, put)
import Data.Functor (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Debug.Trace (traceShowId)
import Text.Megaparsec (MonadParsec (eof, hidden), Parsec, PosState (PosState), SourcePos (SourcePos, sourceColumn), State (State, stateInput, statePosState), errorBundlePretty, getParserState, getSourcePos, mkPos, pstateSourcePos, runParser', setParserState, skipSome, stateOffset, (<?>), unPos)
import Text.Megaparsec.Char (char, newline, space1, string)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Stream (reachOffsetNoLine)
import Text.Megaparsec.Pos (Pos)

type Parser' c = StateT c (Parsec Void Text)

-- Int denotes the mininum indentation level in spaces that is allowed
type Parser = Parser' Pos

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"

whiteSpaceChars :: Parser ()
whiteSpaceChars = void (char ' ' <|> char '\n') <?> "whitespace"

whiteSpace :: Parser ()
whiteSpace = L.space (skipSome whiteSpaceChars) (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")

whiteSpace' :: Parser ()
whiteSpace' = do
    minIndent <- get
    void $ L.indentGuard whiteSpace GT minIndent

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whiteSpace

lexeme' :: Parser a -> Parser a
lexeme' = L.lexeme whiteSpace'

withLineFold :: Parser a -> Parser a
withLineFold p = L.indentLevel >>= flip withLineFold' p

withLineFold' :: Pos -> Parser a -> Parser a
withLineFold' pos p = do
    whiteSpace
    minIndent <- get
    put pos
    a <- p
    put minIndent
    return a

withIndent :: Pos -> Parser a -> Parser a
withIndent indent p = L.indentGuard whiteSpace EQ indent *> p

inParens :: Parser a -> Parser a
inParens = between (lexeme' $ char '(') (char ')') . lexeme'

-- reserved :: [Text]
-- reserved = ["let", "in", "where", "if", "then", "else", "case", "of"]

testParser :: (Show a) => Parser a -> String -> IO ()
testParser p = testParser' p 1

testParser' :: (Show a) => Parser a -> Int -> String -> IO ()
testParser' p level i = do
    let i' = T.pack i
    let state = State i' 0 (PosState i' 1 (SourcePos "" (mkPos 1) (mkPos 1)) (mkPos 4) "") []
    let p' = runStateT p (mkPos level)
    let (state', parsed) = runParser' p' state
    putStrLn "Parsed:"
    case parsed of
        Right ok -> print ok
        Left err -> putStr $ errorBundlePretty err
    putStrLn "\nParsec State"
    print $ stateInput state'
