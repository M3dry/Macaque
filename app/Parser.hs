{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative hiding (many, some)
import Control.Applicative.Combinators (many, some)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Parser.Types qualified as P.T
import Parser.Util (Parser)
import Parser.Util qualified as P.U
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, upperChar)
import Text.Megaparsec.Char.Lexer qualified as L

identifierP :: Parser P.T.Identifier
identifierP =
    (\c cs -> P.T.Identifier $ T.pack (c : cs))
        <$> (char '_' <|> lowerChar)
        <*> many (alphaNumChar <|> char '_' <|> char '\'')
        <* P.U.sc

typeIdentifierP :: Parser P.T.TypeIdentifier
typeIdentifierP =
    (\c cs -> P.T.TypeIdentifier $ T.pack (c : cs))
        <$> upperChar
        <*> many (alphaNumChar <|> char '_' <|> char '\'')
        <* P.U.sc
