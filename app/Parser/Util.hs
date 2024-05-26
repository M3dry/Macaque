{-# LANGUAGE OverloadedStrings #-}

module Parser.Util (Parser, whiteSpace, whiteSpace', lexeme, lexeme', checkIndent, withLineFold, testParser, testParser') where

import Control.Applicative hiding (some)
import Control.Monad.State (StateT (runStateT), get, put, evalStateT)
import Data.Functor (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof), Parsec, PosState (PosState), SourcePos (SourcePos), State (State, stateInput), errorBundlePretty, mkPos, runParser', skipSome, (<?>), pos1)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Pos (Pos)
import Data.Text.IO qualified as TIO
import Text.Megaparsec.State (initialState)

type Parser = StateT (Maybe Pos, Bool) (Parsec Void Text)

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"

whiteSpace :: Parser ()
whiteSpace = do
    (_, sameLine) <- get
    let wsChars = if sameLine then whiteSpaceCharsSameLine else whiteSpaceChars
    L.space
        (skipSome wsChars)
        lineComment
        blockComment

whiteSpaceChars :: Parser ()
whiteSpaceChars = void (char ' ' <|> char '\n') <?> "whitespace"

whiteSpaceCharsSameLine :: Parser ()
whiteSpaceCharsSameLine = void (char ' ') <?> "whitespace"

whiteSpace' :: Parser ()
whiteSpace' = do
    (minimalIndent, _) <- get
    void
        ( case minimalIndent of
            Just minimalIndent' -> L.indentGuard whiteSpace GT minimalIndent'
            Nothing -> L.indentGuard whiteSpace EQ pos1
        )
        <|> eof

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whiteSpace

lexeme' :: Parser a -> Parser a
lexeme' = L.lexeme whiteSpace'

checkIndent :: Parser ()
checkIndent = do
    (minimalIndent, _) <- get
    _ <- case minimalIndent of
        Just minimalIndent' -> L.indentGuard (return ()) GT minimalIndent'
        Nothing -> L.indentGuard (return ()) EQ pos1
    return ()

withLineFold :: Parser a -> Parser a
withLineFold p = do
    whiteSpace
    (minimalIndent, sameLine) <- get
    currIndent <- L.indentLevel
    put (Just currIndent, sameLine)
    a <- p
    put (minimalIndent, sameLine)
    return a

testParser :: (Show a) => Parser a -> IO ()
testParser p = testParser' p (Nothing, False)

testParser' :: (Show a) => Parser a -> (Maybe Pos, Bool) -> IO ()
testParser' p stateP = do
    let evalP = evalStateT p stateP
    input <- TIO.readFile "parser-tests/scratchpad"
    let (state, res) = runParser' evalP $ initialState "scratchpad" input
    putStrLn "Rest of the Input:"
    print $ stateInput state
    putStrLn "\nResult:"
    case res of
        Right a -> print a
        Left err -> putStrLn $ errorBundlePretty err
