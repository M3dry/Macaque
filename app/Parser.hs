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

typeP :: Parser P.T.Type
typeP =
    (P.T.TypeInt <$ P.U.symbol "Int")
        <|> (P.T.TypeChar <$ P.U.symbol "Char")

adtP :: Parser P.T.ADT
adtP = L.nonIndented P.U.scn $ do
    _ <- P.U.symbol "type"
    name <- typeIdentifierP
    body <- optional $ do
        _ <- P.U.symbol "="
        v <- adtVariantP
        vs <- many (P.U.symbol "|" *> adtVariantP)
        return $ v : vs
    return $ P.T.ADT name $ fromMaybe [] body

adtVariantP :: Parser P.T.ADTVariant
adtVariantP =
    ( \ti ts ->
        if null ts
            then P.T.ADTUnit ti
            else P.T.ADTComplex ti ts
    )
        <$> typeIdentifierP
        <*> many typeP
