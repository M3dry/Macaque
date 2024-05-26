{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative hiding (many, some)
import Control.Applicative.Combinators (many, some, someTill)
import Control.Monad (void)
import Data.Text qualified as T
import Parser.Types qualified as P.T
import Parser.Util
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, upperChar)

identifierP :: Parser P.T.Identifier
identifierP =
    lexeme
        ( (\c cs -> P.T.Identifier $ T.pack (c : cs))
            <$> (char '_' <|> lowerChar)
            <*> many (alphaNumChar <|> char '_' <|> char '\'')
        )

typeIdentifierP :: Parser P.T.TypeIdentifier
typeIdentifierP =
    lexeme
        ( (\c cs -> P.T.TypeIdentifier $ T.pack (c : cs))
            <$> upperChar
            <*> many (alphaNumChar <|> char '_' <|> char '\'')
        )

typeP :: Parser P.T.Type
typeP =
    (P.T.TypeSimple <$> typeIdentifierP)
        <|> ( checkIndent
                *> withLineFold
                    ( do
                        _ <- lexeme' (char '(')
                        t <- typeP
                        ts <- someTill (checkIndent *> lexeme' (char ',') *> typeP <* checkIndent) (void $ lexeme (char ')'))
                        return $ P.T.TypeTuple (t : ts)
                    )
            )
