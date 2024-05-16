{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative hiding (many, some)
import Control.Applicative.Combinators (many, some)
import Control.Monad.State (modify)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Parser.Types qualified as P.T
import Parser.Util (Parser)
import Parser.Util qualified as P.U
import Text.Megaparsec (MonadParsec (try))
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
    ( ( \t mt -> case mt of
            Just t' -> P.T.TypeArrow t t'
            Nothing -> t
      )
        <$> typeP'
        <*> optional (try (P.U.indentOnEOL *> P.U.symbol "->" *> P.U.indentOnEOL *> typeP))
    )
  where
    typeP' =
        (P.T.TypeInt <$ P.U.symbol "Int")
            <|> (P.T.TypeChar <$ P.U.symbol "Char")
            <|> (P.T.TypeSimple <$> typeIdentifierP)
            <|> ( P.T.TypeTuple <$> do
                    _ <- P.U.symbol "("
                    P.U.indentOnEOL
                    t <- typeP <* P.U.indentOnEOL
                    ts <- some (P.U.symbol "," *> P.U.indentOnEOL *> typeP <* P.U.indentOnEOL)
                    _ <- P.U.symbol ")"
                    return (t : ts)
                )

adtP :: Parser P.T.ADT
adtP = L.nonIndented P.U.scn $ do
    _ <- P.U.symbol "data"
    modify (+ 1)
    P.U.indentOnEOL
    name <- typeIdentifierP
    P.U.indentOnEOL
    body <- optional $ do
        _ <- P.U.symbol "="
        P.U.indentOnEOL
        v <- adtVariantP
        P.U.indentOnEOL
        vs <- many (P.U.symbol "|" *> P.U.indentOnEOL *> adtVariantP <* P.U.indentOnEOL)
        return (v : vs)
    return $ P.T.ADT name (fromMaybe [] body)

adtVariantP :: Parser P.T.ADTVariant
adtVariantP =
    ( ( \ti mts -> case mts of
            Just ts -> P.T.ADTComplex ti ts
            Nothing -> P.T.ADTUnit ti
      )
        <$> typeIdentifierP
        <*> optional (try (P.U.indentOnEOL *> some (typeP <* P.U.indentOnEOL)))
    )
