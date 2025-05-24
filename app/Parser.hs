{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import AST qualified
import Control.Applicative hiding (many, some)
import Control.Applicative.Combinators (between, many, some, someTill)
import Control.Monad (guard, void)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Set qualified as S
import Data.Text qualified as T
import Parser.AST
import Parser.Util
import Text.Megaparsec (ErrorItem (..), MonadParsec (notFollowedBy, try), chunk, failure, manyTill)
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
                    ( do
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
    AST.LitUnit <$> pos <* lexeme (chunk "()")
        <|> ( ( \p ms ->
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
    (AST.PatLiteral <$> pos <*> try literalP)
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
            <|> try
                ( withLineFold $ do
                    p <- pos
                    _ <- lexeme' (char '(')
                    pat <- lexeme' (patternP True)
                    pats <- someTill (lexeme' (char ',') *> lexeme' (patternP True)) (void $ lexeme (char ')'))
                    return $ AST.PatTuple p (pat : pats)
                )
            <|> between (lexeme' (char '(')) (lexeme (char ')')) (patternP True)

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
            <|> try
                ( withLineFold $ do
                    p <- pos
                    _ <- lexeme' (char '(')
                    expr <- lexeme' expressionP
                    exprs <- someTill (lexeme' (char ',') *> lexeme' expressionP) (void $ lexeme (char ')'))
                    return $ AST.ExprTuple p (expr : exprs)
                )
            <|> try (AST.ExprLiteral <$> pos <*> lexeme literalP)
            <|> between (lexeme' (char '(')) (lexeme (char ')')) expressionP
            <|> (P.T.ExprLiteral <$> lexeme literalP)
    letP = withLineFold $ do
        p <- pos
        keyword' "let"
        idens <- withLineFold $ many $ do
            n <- lexeme' identifierP
            _ <- lexeme' (char '=')
            nExpr <- expressionP
            return (n, nExpr)
        checkIndent
        keyword' "in"
        AST.ExprLet p idens <$> expressionP
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

gadtP :: Parser (AST.GADT Parsing)
gadtP = L.nonIndented whiteSpace . withLineFold $ do
    p <- pos
    keyword' "data"
    name <- typeIdentifierP
    keyword' "where"
    constructors <- inBlock $ do
        many (withLineFold $ constructorP name)
    return $ AST.GADT p name constructors

constructorP :: AST.TypeIdentifier -> Parser (AST.Constructor Parsing)
constructorP ofType = do
    p <- pos
    tIden <- typeIdentifierP
    _ <- lexeme' (char ':')
    signature <- typeP True
    case AST.mkConstructor ofType tIden p signature of
        Just c -> return c
        Nothing -> failure Nothing (S.singleton $ Label ('C' :| "onstructor must create the type it is defined under"))

functionP :: Parser (AST.Function Parsing)
functionP = L.nonIndented whiteSpace $ do
    (name, f) <- withLineFold $ do
        p <- pos
        name <- identifierP
        _ <- lexeme' (char ':')
        signature <- typeP True
        return (name, AST.Function p name signature)
    f <$> some (try $ functionVariantP name)

functionVariantP :: AST.Identifier -> Parser (AST.FunctionVariant Parsing)
functionVariantP name = L.nonIndented whiteSpace . withLineFold $ do
    p <- pos
    name' <- identifierP
    guard (name' == name)
    pats <- manyTill (patternP False) (lexeme' (char '='))
    AST.FunctionVariant p pats <$> expressionP

-- NOTE: Testing only
testP :: Parser ([AST.GADT Parsing], [AST.Function Parsing])
testP = do
    gadts <- many gadtP
    funs <- many functionP
    return (gadts, funs)

testAnyP :: Parser a -> Parser a
testAnyP p = L.nonIndented whiteSpace . withLineFold $ do
    keyword' "any"
    p
