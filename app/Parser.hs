{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative hiding (many, some)
import Control.Applicative.Combinators (between, many, some, someTill)
import Control.Monad (void)
import Data.Text qualified as T
import Parser.Types qualified as P.T
import Parser.Util
import Text.Megaparsec (MonadParsec (notFollowedBy, try), chunk, manyTill)
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, lowerChar, upperChar)
import Text.Megaparsec.Char.Lexer qualified as L
import Type.Reflection (Typeable, eqTypeRep, typeRep, (:~~:) (HRefl))

identifierP :: Parser P.T.Identifier
identifierP =
    notFollowedBy reservedKeywords
        *> lexeme
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
    keyword' "data"
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

literalP :: Parser P.T.Literal
literalP =
    ( ( \ms ->
            ( case ms of
                Just '-' -> P.T.LitNumber False
                _ -> P.T.LitNumber True
            )
                . T.pack
      )
        <$> optional (char '-' <|> char '+')
        <*> some digitChar
    )
        <|> ( P.T.LitChar
                <$> (char '\'' *> L.charLiteral <* char '\'')
            )
        <|> ( P.T.LitString . T.pack
                <$> (char '"' *> manyTill L.charLiteral (char '"'))
            )

patternP :: forall safe. (Typeable safe) => Bool -> Parser (P.T.Pattern safe)
patternP inParens = case typeRep @safe `eqTypeRep` typeRep @'False of
    Just HRefl ->
        (P.T.PatLiteral <$> literalP)
            <|> safeP @'False inParens
    Nothing -> safeP inParens
  where
    safeP :: forall safe'. (Typeable safe') => Bool -> Parser (P.T.Pattern safe')
    safeP inParens' =
        (P.T.PatIgnore <$ lexeme (char '_'))
            <|> ( ( \i mp -> case mp of
                        Just p -> P.T.PatCapture i p
                        Nothing -> P.T.PatVariable i
                )
                    <$> identifierP
                    <*> optional
                        ( do
                            checkIndent
                            _ <- lexeme' (char '@')
                            patternP @safe' False
                        )
                )
            <|> ( do
                    ti <- typeIdentifierP
                    pats <- if inParens' then many (checkIndent *> patternP @safe' inParens') else pure []
                    return $ P.T.PatConstructor ti pats
                )
            <|> between (lexeme' (char '(')) (lexeme (char ')')) (patternP @safe' True)

-- NOTE: Testing only
testExprP :: Parser P.T.Expression
testExprP = L.nonIndented whiteSpace . withLineFold $ do
    keyword' "expr"
    expressionP

expressionP :: Parser P.T.Expression
expressionP = typed >>= application
  where
    application :: P.T.Expression -> Parser P.T.Expression
    application expr = do
        mExpr <- optional (checkIndent *> try typed)
        case mExpr of
            Just expr2 -> application (P.T.ExprApplied expr expr2)
            Nothing -> pure expr
    typed =
        ( \e tm -> case tm of
            Just t -> P.T.ExprTyped e t
            Nothing -> e
        )
            <$> expressionP'
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
            <|> (P.T.ExprVar <$> identifierP)
            <|> (P.T.ExprTypeConstructor <$> typeIdentifierP)
            <|> lambdaP
            <|> between (lexeme' (char '(')) (lexeme (char ')')) expressionP
            <|> (P.T.ExprLiteral <$> literalP)
    letP = withLineFold $ do
        keyword' "let"
        (n, nExpr) <- withLineFold $ do
            n <- lexeme' identifierP
            _ <- lexeme' (char '=')
            nExpr <- expressionP
            return (n, nExpr)
        checkIndent
        keyword' "in"
        expr <- expressionP
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
                    pat <- lexeme' (patternP True)
                    _ <- lexeme' (chunk "->")
                    branchExpr <- expressionP
                    return (pat, branchExpr)
                )
        return $ P.T.ExprCase expr branches
    lambdaP = withLineFold $ do
        _ <- lexeme' (char '\\')
        pats <- someTill (lexeme' $ patternP False) (lexeme' (chunk "->"))
        expr <- expressionP
        return $ P.T.ExprLambda pats expr

functionSignatureP :: Parser P.T.FunctionSignature
functionSignatureP = L.nonIndented whiteSpace . withLineFold $ do
    name <- lexeme' identifierP
    _ <- lexeme' (char ':')
    t <- typeP True
    return $ P.T.FunctionSignature{name = name, signature = t}

functionVariantP :: Parser P.T.FunctionVariant
functionVariantP = L.nonIndented whiteSpace . withLineFold $ do
        name <- lexeme' identifierP
        pats <- manyTill (lexeme' $ patternP False) (lexeme' $ char '=')
        expr <- expressionP
        return $ P.T.FunctionVariant{name=name, patterns=pats, expression=expr}

topLevelP :: Parser P.T.TopLevel
topLevelP = (P.T.TopLvlADT <$> adtP) <|> try (P.T.TopLvlFunSig <$> functionSignatureP) <|> (P.T.TopLvlFunVariant <$> functionVariantP)

fileP :: Parser P.T.File
fileP = do
    topLevelDefs <- many topLevelP
    return $ P.T.File{topLevel=topLevelDefs}
