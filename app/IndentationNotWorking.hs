{-# LANGUAGE OverloadedStrings #-}

module IndentationNotWorking where

import Control.Applicative
import Control.Monad (void)
import Control.Monad.State (MonadState (get, put), StateT, evalStateT)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof), Parsec, Pos, chunk, pos1, skipSome, (<?>), runParser', State (stateInput), errorBundlePretty, mkPos)
import Text.Megaparsec.Char (alphaNumChar, char)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.State (initialState)

type Parser = StateT (Maybe Pos, Bool) (Parsec Void Text)

whiteSpaceChars :: Parser ()
whiteSpaceChars = void (char ' ' <|> char '\n') <?> "whitespace"

whiteSpaceCharsSameLine :: Parser ()
whiteSpaceCharsSameLine = void (char ' ') <?> "whitespace"

whiteSpace :: Parser ()
whiteSpace = do
    (_, sameLine) <- get
    let wsChars = if sameLine then whiteSpaceCharsSameLine else whiteSpaceChars
    L.space
        (skipSome wsChars)
        (L.skipLineComment "--")
        empty

whiteSpaceWithIndent :: Parser ()
whiteSpaceWithIndent = do
    (minimalIndent, _) <- get
    void
        ( case minimalIndent of
            Just minimalIndent' -> L.indentGuard whiteSpace GT minimalIndent'
            Nothing -> L.indentGuard whiteSpace EQ pos1
        )
        <|> eof

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whiteSpace

lexemeWithIndent :: Parser a -> Parser a
lexemeWithIndent = L.lexeme whiteSpaceWithIndent

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

wordP :: Parser Text
wordP = T.pack <$> lexeme (some alphaNumChar)

assignP :: Parser (Text, Text)
assignP = L.nonIndented whiteSpace . withLineFold $ do
    name <- wordP
    checkIndent
    value <- do
        _ <- lexemeWithIndent (chunk "=")
        wordP
    return (name, value)

assignsP :: Parser [(Text, Text)]
assignsP = some assignP

senteceIndented :: Pos -> Parser [Text]
senteceIndented pos = do
    (minimalIndent, sameLine) <- get
    put (Just pos, True)
    words' <- some (checkIndent *> wordP)
    put (minimalIndent, sameLine)
    whiteSpace
    return words'

run :: IO ()
run = do
    let assignsP' = evalStateT ((,) <$> (whiteSpaceWithIndent *> assignsP) <*> senteceIndented (mkPos 4)) (Nothing, False)
    input <- readFile "parser-tests/indentation-not-working"
    let (state, res) = runParser' assignsP' $ initialState "indentation-not-working" $ T.pack input
    putStrLn "Rest of the Input:"
    print $ stateInput state
    putStrLn "\nResult:"
    case res of
        Right a -> print a
        Left err -> print $ errorBundlePretty err
