{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative hiding (many, some)
import Control.Applicative.Combinators (between, many, some, someTill)
import Control.Monad (void)
import Data.Text qualified as T
import Parser.Types qualified as P.T
import Parser.Util
import Text.Megaparsec (MonadParsec (try), chunk)
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, upperChar)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug (MonadParsecDbg(dbg))

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

adtP :: Parser P.T.ADT
adtP = L.nonIndented whiteSpace . withLineFold $ do
    _ <- lexeme' (chunk "data")
    name <- lexeme typeIdentifierP
    constructors <-
        ( do
                checkIndent
                _ <- lexeme' (char '=')
                c <- adtConstructor
                cs <- many (checkIndent *> lexeme' (char '|') *> adtConstructor)
                return $ c : cs
            )
            <|> pure []
    return P.T.ADT{name = name, constructors = constructors}

adtConstructor :: Parser P.T.ADTConstructor
adtConstructor =
    P.T.ADTNormal
        <$> typeIdentifierP
        <*> many (checkIndent *> typeP False)

typeP :: Bool -> Parser P.T.Type
typeP inParens =
    ( \t mt -> case mt of
        Just t' -> P.T.TypeArrow t t'
        Nothing -> t
    )
        <$> typeP'
        <*> if inParens
            then
                optional
                    ( try $ do
                        checkIndent
                        _ <- lexeme' (chunk "->")
                        typeP True
                    )
            else pure Nothing
  where
    typeP' =
        (P.T.TypeSimple <$> typeIdentifierP)
            <|> try
                ( withLineFold
                    ( do
                        _ <- lexeme' (char '(')
                        t <- lexeme' (typeP True)
                        ts <-
                            someTill
                                (lexeme' (char ',') *> lexeme' (typeP True))
                                (void $ lexeme (char ')'))
                        return $ P.T.TypeTuple (t : ts)
                    )
                )
            <|> between
                (lexeme' (char '('))
                (lexeme (char ')'))
                (typeP True)

patternP :: Parser P.T.Pattern
patternP =
    ( ( \i mp -> case mp of
            Just p -> P.T.PatCapture i p
            Nothing -> P.T.PatVariable i
      )
        <$> identifierP
        <*> optional
            ( do
                checkIndent
                _ <- lexeme' (char '@')
                patternP
            )
    )
        <|> ( do
                ti <- typeIdentifierP
                pats <- many (checkIndent *> patternP)
                return $ P.T.PatConstructor ti pats
            )

testExprP :: Parser P.T.Expression
testExprP = L.nonIndented whiteSpace . withLineFold $ do
    keyword' "expr"
    expressionP

expressionP :: Parser P.T.Expression
expressionP = dbg "typed" typed
  where
    application :: P.T.Expression -> Parser P.T.Expression
    application expr = dbg "application" $ do
        mExpr <- optional (try typed)
        case mExpr of
            Just expr2 -> application (P.T.ExprApplied expr expr2)
            Nothing -> pure expr
    typed =
        ( ( \e tm -> case tm of
                Just t -> P.T.ExprTyped e t
                Nothing -> e
          )
            <$> expressionP'
            <*> optional
                ( withLineFold $ do
                    checkIndent
                    _ <- lexeme' (chunk "::")
                    typeP True
                )
        )
    expressionP' =
        letP
            <|> ifElseP
            <|> caseP
            <|> (P.T.ExprVar <$> identifierP)
            <|> (P.T.ExprTypeConstructor <$> typeIdentifierP)
            <|> between (lexeme' (char '(')) (lexeme (char ')')) expressionP
    letP = withLineFold $ do
        keyword' "let"
        (n, nExpr) <- withLineFold $ do
            n <- lexeme' identifierP
            _ <- lexeme' (char '=')
            nExpr <- expressionP
            return (n, nExpr)
        checkIndent
        keyword' "in"
        expr <- dbg "let expr" expressionP
        return $ P.T.ExprLet n nExpr expr
    ifElseP = withLineFold $ do
        keyword' "if"
        cond <- lexeme' expressionP
        keyword' "then"
        trueBranch <- lexeme' expressionP
        keyword' "else"
        falseBranch <- lexeme expressionP
        return $ P.T.ExprIfElse cond trueBranch falseBranch
    caseP = withLineFold $ do
        keyword' "case"
        expr <- lexeme' expressionP
        keyword' "of"
        branches <-
            some
                ( withLineFold $ do
                    pat <- lexeme' patternP
                    _ <- lexeme' (chunk "->")
                    branchExpr <- expressionP
                    return (pat, branchExpr)
                )
        return $ P.T.ExprCase expr branches

functionSignatureP :: Parser P.T.FunctionSignature
functionSignatureP = L.nonIndented whiteSpace . withLineFold $ do
    name <- lexeme' identifierP
    _ <- lexeme' (char ':')
    t <- typeP True
    return $ P.T.FunctionSignature{name = name, signature = t}
