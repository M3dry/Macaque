{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative hiding (many, some)
import Control.Applicative.Combinators (many, some, someTill)
import Control.Monad.State (get, modify)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Debug.Trace (traceShowId, trace, traceM)
import Parser.Types qualified as P.T
import Parser.Util (Parser)
import Parser.Util qualified as P.U
import Text.Megaparsec (MonadParsec (try, getParserState), chunk, pos1, getSourcePos, sourcePosPretty)
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, upperChar)
import Text.Megaparsec.Char.Lexer qualified as L

identifierP :: Parser P.T.Identifier
identifierP =
    P.U.lexeme'
        ( (\c cs -> P.T.Identifier $ T.pack (c : cs))
            <$> (char '_' <|> lowerChar)
            <*> many (alphaNumChar <|> char '_' <|> char '\'')
        )

typeIdentifierP :: Parser P.T.TypeIdentifier
typeIdentifierP =
    P.U.lexeme'
        ( (\c cs -> P.T.TypeIdentifier $ T.pack (c : cs))
            <$> upperChar
            <*> many (alphaNumChar <|> char '_' <|> char '\'')
        )

-- typeP :: Parser P.T.Type
-- typeP =
--     ( ( \t mt -> case mt of
--             Just t' -> P.T.TypeArrow t t'
--             Nothing -> t
--       )
--         <$> typeP'
--         <*> optional (try (P.U.indentOnEOL *> P.U.symbol "->" *> P.U.indentOnEOL *> typeP))
--     )
--   where
--     typeP' =
--         (P.T.TypeInt <$ P.U.symbol "Int")
--             <|> (P.T.TypeChar <$ P.U.symbol "Char")
--             <|> (P.T.TypeSimple <$> typeIdentifierP)
--             <|> ( P.T.TypeTuple <$> do
--                     _ <- P.U.symbol "("
--                     P.U.indentOnEOL
--                     t <- typeP <* P.U.indentOnEOL
--                     ts <- some (P.U.symbol "," *> P.U.indentOnEOL *> typeP <* P.U.indentOnEOL)
--                     _ <- P.U.symbol ")"
--                     return (t : ts)
--                 )

-- adtP :: Parser P.T.ADT
-- adtP = L.nonIndented P.U.scn $ do
--     _ <- P.U.symbol "data"
--     modify (+ 1)
--     P.U.indentOnEOL
--     name <- typeIdentifierP
--     P.U.indentOnEOL
--     body <- optional $ do
--         _ <- P.U.symbol "="
--         P.U.indentOnEOL
--         v <- adtVariantP
--         P.U.indentOnEOL
--         vs <- many (P.U.symbol "|" *> P.U.indentOnEOL *> adtVariantP <* P.U.indentOnEOL)
--         return (v : vs)
--     return $ P.T.ADT name (fromMaybe [] body)

-- adtVariantP :: Parser P.T.ADTVariant
-- adtVariantP =
--     ( ( \ti mts -> case mts of
--             Just ts -> P.T.ADTComplex ti ts
--             Nothing -> P.T.ADTUnit ti
--       )
--         <$> typeIdentifierP
--         <*> optional (try (P.U.indentOnEOL *> some (typeP <* P.U.indentOnEOL)))
--     )

adtP :: Parser P.T.ADT
adtP = do
    !_ <- do
        indentLevel <- L.indentLevel
        sourcePos <- getSourcePos
        minIndent <- get
        traceM ("indentLevel: " <> show indentLevel)
        traceM ("sourcePos: " <> show sourcePos)
        traceM ("minIndent: " <> show minIndent)
        return ()
    L.nonIndented P.U.whiteSpace $ do
        _ <- P.U.lexeme' $ chunk "data"
        name <- typeIdentifierP
        constructors <- optional $ do
            _ <- P.U.lexeme' $ chunk "="
            c <- adtConstructorP
            cs <- many (P.U.lexeme' (chunk "|") *> adtConstructorP)
            return (c : cs)
        return $ P.T.ADT{P.T.name, P.T.constructors = fromMaybe [] constructors}

adtConstructorP :: Parser P.T.ADTConstructor
adtConstructorP = do
    name <- typeIdentifierP
    inputs <- many typeP
    return $ P.T.ADTNormal name inputs

typeP :: Parser P.T.Type
typeP =
    (P.T.TypeInt <$ P.U.lexeme' (chunk "Int"))
        <|> (P.T.TypeChar <$ P.U.lexeme' (chunk "Char"))
        <|> (P.T.TypeSimple <$> typeIdentifierP)

-- <|> ( P.U.withLineFold $ do
--         _ <- chunk "("
--         t <- typeP
--         ts <- someTill (P.U.lexeme' (chunk ",") *> typeP) (P.U.lexeme' (chunk ")"))
--         return $ P.T.TypeTuple (t : ts)
--     )
