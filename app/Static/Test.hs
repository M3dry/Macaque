{-# LANGUAGE OverloadedStrings #-}

module Static.Test where

import Parser.Util (run)
import Parser (testExprP)
import Static.TypeChecker (typeCheckExpr)
import Static.Types (Symbols(..), runTypeChecker, TypeChecker, TaggedType(..))
import qualified Data.Map.Strict as M
import Parser.Types (Type(..), TypeIdentifier (TypeIdentifier), Identifier (Identifier))
import Data.Text (Text)
import Text.Pretty.Simple (pPrint)

simpleType :: Text -> Type
simpleType = TypeSimple . TypeIdentifier

sigs = M.fromList
    [(Identifier "add",
      TypeArrow (simpleType "Int")
                (TypeArrow (simpleType "Int")
                           (simpleType "Int")))
    , (Identifier "hundo", TypeSimple $ TypeIdentifier "Int")]

typeCheckRunner :: TypeChecker ret -> Maybe ret
typeCheckRunner = runTypeChecker Symbols{constructors=M.empty,signatures=sigs} M.empty

typeExpr :: FilePath -> IO ()
typeExpr filePath = do
    expr <- run testExprP filePath
    pPrint $ typeCheckRunner $ typeCheckExpr expr
