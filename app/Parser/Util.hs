{-# LANGUAGE OverloadedStrings #-}

module Parser.Util (Parser, whiteSpace, whiteSpace', lexeme, lexeme', keyword, keyword', reserved, reservedKeywords, checkIndent, withLineFold, testParser, run, run') where

import Control.Applicative hiding (some)
import Control.Applicative.Combinators (choice)
import Control.Monad.State (StateT (runStateT), evalStateT, get, put)
import Data.Functor (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof, lookAhead), Parsec, PosState (PosState), SourcePos (SourcePos), State (State, stateInput), chunk, errorBundlePretty, mkPos, pos1, runParser', skipSome, (<?>))
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Pos (Pos)
import Text.Megaparsec.State (initialState)
import Text.Pretty.Simple (pPrint)

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

keyword :: Text -> Parser ()
keyword k = lexeme (chunk k *> lookAhead whiteSpaceChars)

keyword' :: Text -> Parser ()
keyword' k = lexeme' (chunk k *> lookAhead whiteSpaceChars)

reserved :: [Text]
reserved = ["data", "let", "in", "if", "then", "else", "case", "of"]

reservedKeywords :: Parser ()
reservedKeywords = choice $ keyword <$> reserved

checkIndent :: Parser ()
checkIndent = do
    (minimalIndent, _) <- get
    _ <- case minimalIndent of
        Just minimalIndent' -> L.indentGuard (return ()) GT minimalIndent'
        Nothing -> L.indentGuard (return ()) EQ pos1
    return ()

withLineFold :: Parser a -> Parser a
withLineFold p = do
    (minimalIndent, sameLine) <- get
    currIndent <- L.indentLevel
    put (Just currIndent, sameLine)
    a <- p
    put (minimalIndent, sameLine)
    return a

testParser :: (Show a) => Parser a -> IO ()
testParser p = run p "scratchpad"

run :: (Show a) => Parser a -> FilePath -> IO ()
run p file = run' p (Nothing, False) $ "parser-tests/" ++ file

run' :: (Show a) => Parser a -> (Maybe Pos, Bool) -> FilePath -> IO ()
run' p stateP file = do
    let evalP = evalStateT p stateP
    input <- TIO.readFile file
    let (state, res) = runParser' evalP $ initialState "scratchpad" input
    putStrLn "Rest of the Input:"
    print $ stateInput state
    putStrLn "\nResult:"
    case res of
        Right a -> pPrint a
        Left err -> putStrLn $ errorBundlePretty err
