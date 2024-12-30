{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import AST qualified
import Control.Applicative hiding (many, some)
import Control.Applicative.Combinators (between, many, some, someTill)
import Control.Monad (void)
import Data.Text qualified as T
import Parser.AST
import Parser.Util
import Text.Megaparsec (MonadParsec (notFollowedBy, try), chunk, manyTill)
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, lowerChar, upperChar)
import Text.Megaparsec.Char.Lexer qualified as L

identifierP :: Parser AST.Identifier
identifierP =
    notFollowedBy reservedKeywords
        *> lexeme
            ( (\c cs -> AST.Identifier $ T.pack (c : cs))
                <$> (char '_' <|> lowerChar)
                <*> many (alphaNumChar <|> char '_' <|> char '\'')
            )

typeIdentifierP :: Parser AST.TypeIdentifier
typeIdentifierP =
    lexeme
        ( (\c cs -> AST.TypeIdentifier $ T.pack (c : cs))
            <$> upperChar
            <*> many (alphaNumChar <|> char '_' <|> char '\'')
        )

typeP :: Bool -> Parser (AST.Type Parsing)
typeP inParens =
    ( \p t mt -> case mt of
        Just t' -> AST.TypeArrow p t t'
        Nothing -> t
    )
        <$> pos
        <*> typeP'
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
        (AST.TypeSimple <$> pos <*> typeIdentifierP)
            <|> try
                ( withLineFold
                    ( do
                        p <- pos
                        _ <- lexeme' (char '(')
                        t <- lexeme' (typeP True)
                        ts <-
                            someTill
                                (lexeme' (char ',') *> lexeme' (typeP True))
                                (void $ lexeme (char ')'))
                        return $ AST.TypeTuple p (t : ts)
                    )
                )
            <|> between
                (lexeme' (char '('))
                (lexeme (char ')'))
                (typeP True)

literalP :: Parser (AST.Literal Parsing)
literalP =
    ( ( \p ms ->
            ( case ms of
                Just '-' -> AST.LitNumber p AST.Negative
                _ -> AST.LitNumber p AST.Positive
            )
                . T.pack
      )
        <$> pos
        <*> optional (char '-' <|> char '+')
        <*> some digitChar
    )
        <|> ( AST.LitChar
                <$> pos
                <*> (char '\'' *> L.charLiteral <* char '\'')
            )
        <|> ( (\p -> AST.LitString p . T.pack)
                <$> pos
                <*> (char '"' *> manyTill L.charLiteral (char '"'))
            )

patternP :: Bool -> Parser (AST.Pattern Parsing)
patternP inParens =
    (AST.PatLiteral <$> pos <*> literalP)
        <|> safeP inParens
  where
    safeP inParens' =
        (AST.PatIgnore <$> pos <* lexeme (char '_'))
            <|> ( ( \p i mp -> case mp of
                        Just pat -> AST.PatCapture p i pat
                        Nothing -> AST.PatVariable p i
                  )
                    <$> pos
                    <*> identifierP
                    <*> optional
                        ( do
                            checkIndent
                            _ <- lexeme' (char '@')
                            patternP False
                        )
                )
            <|> ( do
                    p <- pos
                    ti <- typeIdentifierP
                    pats <- if inParens' then many (checkIndent *> patternP inParens') else pure []
                    return $ AST.PatConstructor p ti pats
                )
            <|> between (lexeme' (char '(')) (lexeme (char ')')) (patternP True)

--
-- NOTE: Testing only
testExprP :: Parser (AST.Expression Parsing)
testExprP = L.nonIndented whiteSpace . withLineFold $ do
    keyword' "expr"
    expressionP

expressionP :: Parser (AST.Expression Parsing)
expressionP = typed >>= application
  where
    application :: AST.Expression Parsing -> Parser (AST.Expression Parsing)
    application expr = do
        p <- pos
        mExpr <- optional (checkIndent *> try typed)
        case mExpr of
            Just expr2 -> application (AST.ExprApplied p expr expr2)
            Nothing -> pure expr
    typed =
        ( \p e tm -> case tm of
            Just t -> AST.ExprTyped p e t
            Nothing -> e
        )
            <$> pos
            <*> expressionP'
            <*> optional
                ( withLineFold $ do
                    checkIndent
                    _ <- lexeme' (chunk ":")
                    typeP True
                )
    expressionP' =
        letP
            <|> ifElseP
            <|> caseP
            <|> (AST.ExprVar <$> pos <*> identifierP)
            <|> (AST.ExprTypeConstructor <$> pos <*> typeIdentifierP)
            <|> lambdaP
            <|> between (lexeme' (char '(')) (lexeme (char ')')) expressionP
            <|> (AST.ExprLiteral <$> pos <*> lexeme literalP)
    letP = withLineFold $ do
        p <- pos
        keyword' "let"
        (n, nExpr) <- withLineFold $ do
            n <- lexeme' identifierP
            _ <- lexeme' (char '=')
            nExpr <- expressionP
            return (n, nExpr)
        checkIndent
        keyword' "in"
        AST.ExprLet p n nExpr <$> expressionP
    ifElseP = withLineFold $ do
        p <- pos
        keyword' "if"
        cond <- lexeme' expressionP
        keyword' "then"
        trueBranch <- lexeme' expressionP
        keyword' "else"
        falseBranch <- lexeme expressionP
        return $ AST.ExprIfElse p cond trueBranch falseBranch
    caseP = withLineFold $ do
        p <- pos
        keyword' "case"
        expr <- lexeme' expressionP
        keyword' "of"
        branches <-
            some
                ( withLineFold $ do
                    pat <- lexeme' (patternP True)
                    _ <- lexeme' (chunk "->")
                    branchExpr <- expressionP
                    return (pat, branchExpr)
                )
        return $ AST.ExprCase p expr branches
    lambdaP = withLineFold $ do
        p <- pos
        _ <- lexeme' (char '\\')
        pats <- someTill (lexeme' $ patternP False) (lexeme' (chunk "->"))
        AST.ExprLambda p pats <$> expressionP

-- functionSignatureP :: Parser P.T.FunctionSignature
-- functionSignatureP = L.nonIndented whiteSpace . withLineFold $ do
--     name <- lexeme' identifierP
--     _ <- lexeme' (char ':')
--     t <- typeP True
--     return $ P.T.FunctionSignature{name = name, signature = t}
--
-- functionVariantP :: Parser P.T.FunctionVariant
-- functionVariantP = L.nonIndented whiteSpace . withLineFold $ do
--         name <- lexeme' identifierP
--         pats <- manyTill (lexeme' $ patternP False) (lexeme' $ char '=')
--         expr <- expressionP
--         return $ P.T.FunctionVariant{name=name, patterns=pats, expression=expr}
--
-- topLevelP :: Parser P.T.TopLevel
-- topLevelP = (P.T.TopLvlADT <$> adtP) <|> try (P.T.TopLvlFunSig <$> functionSignatureP) <|> (P.T.TopLvlFunVariant <$> functionVariantP)
--
-- fileP :: Parser P.T.File
-- fileP = do
--     topLevelDefs <- many topLevelP
--     return $ P.T.File{topLevel=topLevelDefs}
