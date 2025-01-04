{-# LANGUAGE OverloadedStrings #-}

module Parser.Util (Parser, whiteSpace, whiteSpace', lexeme, lexeme', keyword, keyword', reserved, reservedKeywords, checkIndent, withLineFold, inBlock, pos, testParser, run, run') where

import Control.Applicative hiding (some)
import Control.Applicative.Combinators (choice)
import Control.Monad.State (StateT, evalStateT, get, put)
import Data.Functor (void)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import Text.Megaparsec (
    MonadParsec (eof, lookAhead),
    Parsec,
    SourcePos,
    State (stateInput),
    chunk,
    errorBundlePretty,
    getSourcePos,
    pos1,
    runParser',
    skipSome,
    (<?>), try,
 )
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Pos (Pos)
import Text.Megaparsec.State (initialState)

type Parser = StateT (Maybe Pos, Bool, Bool) (Parsec Void Text)

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"

whiteSpace :: Parser ()
whiteSpace = do
    (_, sameLine, _) <- get
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
    (minimalIndent, _, block) <- get
    try (lookAhead (whiteSpace *> eof))
        <|> void
            ( case minimalIndent of
                Just minimalIndent' | block -> L.indentGuard whiteSpace EQ minimalIndent'
                Just minimalIndent' -> L.indentGuard whiteSpace GT minimalIndent'
                Nothing -> L.indentGuard whiteSpace EQ pos1
            )

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whiteSpace

lexeme' :: Parser a -> Parser a
lexeme' = L.lexeme whiteSpace'

keyword :: Text -> Parser ()
keyword k = lexeme (chunk k *> lookAhead whiteSpaceChars)

keyword' :: Text -> Parser ()
keyword' k = lexeme' (chunk k *> lookAhead whiteSpaceChars)

reserved :: [Text]
reserved = ["data", "where", "let", "in", "if", "then", "else", "case", "of"]

reservedKeywords :: Parser ()
reservedKeywords = choice $ keyword <$> reserved

checkIndent :: Parser ()
checkIndent = do
    (minimalIndent, _, block) <- get
    _ <- case minimalIndent of
        Just minimalIndent' | block -> L.indentGuard (return ()) EQ minimalIndent'
        Just minimalIndent' -> L.indentGuard (return ()) GT minimalIndent'
        Nothing -> L.indentGuard (return ()) EQ pos1
    return ()

withLineFold :: Parser a -> Parser a
withLineFold p = do
    (minimalIndent, sameLine, block) <- get
    currIndent <- L.indentLevel
    put (Just currIndent, sameLine, False)
    a <- p
    put (minimalIndent, sameLine, block)
    return a

inBlock :: Parser a -> Parser a
inBlock p = do
    (minimalIndent, sameLine, block) <- get
    currIndent <- L.indentLevel
    put (Just currIndent, sameLine, True)
    a <- p
    put (minimalIndent, sameLine, block)
    return a

pos :: Parser SourcePos
pos = getSourcePos

testParser :: (Show a) => Parser a -> IO a
testParser p = run p "scratchpad"

run :: (Show a) => Parser a -> FilePath -> IO a
run p file = run' p (Nothing, False, False) $ "parser-tests/" ++ file

run' :: (Show a) => Parser a -> (Maybe Pos, Bool, Bool) -> FilePath -> IO a
run' p stateP file = do
    let evalP = evalStateT p stateP
    input <- TIO.readFile file
    let (state, res) = runParser' evalP $ initialState "scratchpad" input
    putStrLn "Rest of the Input:"
    print $ stateInput state
    putStrLn "\nResult:"
    case res of
        Right a -> return a
        Left err -> do
            putStrLn $ errorBundlePretty err
            fail ""
