{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module AST where

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Kind qualified as K
import Data.Text (Text)
import Unsafe.Coerce (unsafeCoerce)

type family AllTags a :: [K.Type]

type family AllEq fTagged :: K.Constraint where
    AllEq fTagged = AllEq' (AllTags fTagged)

type family AllEq' (xs :: [K.Type]) :: K.Constraint where
    AllEq' (x ': y ': xs) = (x ~ y, AllEq' (y ': xs))
    AllEq' _ = ()

type family TypeOfFirst (xs :: [K.Type]) :: K.Type where
    TypeOfFirst (x ': _) = x

type family ApplyConstraint2 (xs :: [K.Type]) (ys :: [K.Type]) (c :: K.Type -> K.Type -> K.Constraint) :: K.Constraint where
    ApplyConstraint2 (x ': xs) (y ': ys) c = (c x y, ApplyConstraint2 xs ys c)
    ApplyConstraint2 '[] '[] _ = ()

class Extract f where
    extractTag :: (AllEq (f tag)) => f tag -> TypeOfFirst (AllTags (f tag))

newtype Identifier = Identifier Text
    deriving (Show, Eq, Ord)

newtype TypeIdentifier = TypeIdentifier Text
    deriving (Show, Eq, Ord)

convert :: (ApplyConstraint2 (AllTags (f tag1)) (AllTags (f tag2)) (~)) => f tag1 -> f tag2
convert = unsafeCoerce -- fuck it we ball

data Type tag
    = TypeArrow (TypeArrow tag) (Type tag) (Type tag)
    | TypeTuple (TypeTuple tag) [Type tag]
    | TypeSimple (TypeSimple tag) TypeIdentifier
    | TypeHole (TypeHole tag)
    | TypeUnit (TypeUnit tag)

deriving instance
    ( Show (TypeArrow tag)
    , Show (TypeTuple tag)
    , Show (TypeSimple tag)
    , Show (TypeHole tag)
    , Show (TypeUnit tag)
    ) =>
    Show (Type tag)

type family TypeArrow tag
type family TypeTuple tag
type family TypeSimple tag
type family TypeHole tag
type family TypeUnit tag
type instance AllTags (Type tag) = '[TypeArrow tag, TypeTuple tag, TypeSimple tag, TypeHole tag, TypeUnit tag]

makeBaseFunctor ''Type

instance Extract Type where
    extractTag (TypeArrow tag _ _) = tag
    extractTag (TypeTuple tag _) = tag
    extractTag (TypeSimple tag _) = tag
    extractTag (TypeHole tag) = tag
    extractTag (TypeUnit tag) = tag

-- modifyTypeTags ::
--     forall tag1 tag2.
--     (AllEq' "Type" tag1 tag2) =>
--     (TypeUnit tag1 -> TypeUnit tag2) ->
--     Type tag1 ->
--     Type tag2
-- modifyTypeTags f = cata alg
--   where
--     alg :: TypeF tag1 (Type tag2) -> Type tag2
--     alg (TypeArrowF tag t1 t2) = TypeArrow (f tag) t1 t2
--     alg (TypeTupleF tag ts) = TypeTuple (f tag) ts
--     alg (TypeSimpleF tag typeIden) = TypeSimple (f tag) typeIden
--     alg (TypeHoleF tag) = TypeHole (f tag)
--     alg (TypeUnitF tag) = TypeHole (f tag)

data Sign = Positive | Negative
    deriving (Show)

data Literal tag
    = LitNumber (LitNumber tag) Sign Text
    | LitString (LitString tag) Text
    | LitChar (LitChar tag) Char

deriving instance
    ( Show (LitNumber tag)
    , Show (LitString tag)
    , Show (LitChar tag)
    ) =>
    Show (Literal tag)

type family LitNumber tag
type family LitString tag
type family LitChar tag
type instance AllTags (Literal tag) = '[LitNumber tag, LitString tag, LitChar tag]

instance Extract Literal where
    extractTag (LitNumber tag _ _) = tag
    extractTag (LitString tag _) = tag
    extractTag (LitChar tag _) = tag

--
-- modifyLitTags ::
--     forall tag1 tag2.
--     (AllEq' "Literal" tag1 tag2) =>
--     (LitNumber tag1 -> LitNumber tag2) ->
--     Literal tag1 ->
--     Literal tag2
-- modifyLitTags f (LitNumber tag sign num) = LitNumber (f tag) sign num
-- modifyLitTags f (LitString tag str) = LitString (f tag) str
-- modifyLitTags f (LitChar tag ch) = LitChar (f tag) ch

data Pattern tag
    = PatVariable (PatVariable tag) Identifier
    | PatCapture (PatCapture tag) Identifier (Pattern tag)
    | PatConstructor (PatConstructor tag) TypeIdentifier [Pattern tag]
    | PatLiteral (PatLiteral tag) (Literal tag)
    | PatIgnore (PatIgnore tag)

deriving instance
    ( Show (PatVariable tag)
    , Show (PatCapture tag)
    , Show (PatConstructor tag)
    , Show (PatLiteral tag)
    , Show (PatIgnore tag)
    , Show (Literal tag)
    ) =>
    Show (Pattern tag)

type family PatVariable tag
type family PatCapture tag
type family PatConstructor tag
type family PatLiteral tag
type family PatIgnore tag
type instance
    AllTags (Pattern tag) =
        '[ PatVariable tag
         , PatCapture tag
         , PatConstructor tag
         , PatLiteral tag
         , PatIgnore tag
         ]

makeBaseFunctor ''Pattern

instance Extract Pattern where
    extractTag (PatVariable tag _) = tag
    extractTag (PatCapture tag _ _) = tag
    extractTag (PatConstructor tag _ _) = tag
    extractTag (PatLiteral tag _) = tag
    extractTag (PatIgnore tag) = tag

-- type AllPatEQ tag1 tag2 = (AllEq' "Pattern" tag1 tag2, AllEq' "Literal" tag1 tag2, PatVariable tag1 ~ LitNumber tag1, PatVariable tag2 ~ LitNumber tag2)
-- modifyPatTags ::
--     forall tag1 tag2.
--     (AllPatEQ tag1 tag2) =>
--     (PatVariable tag1 -> PatVariable tag2) ->
--     Pattern tag1 ->
--     Pattern tag2
-- modifyPatTags f = cata alg
--   where
--     alg :: PatternF tag1 (Pattern tag2) -> Pattern tag2
--     alg (PatVariableF tag iden) = PatVariable (f tag) iden
--     alg (PatCaptureF tag iden pat) = PatCapture (f tag) iden pat
--     alg (PatConstructorF tag typeIden pats) = PatConstructor (f tag) typeIden pats
--     alg (PatLiteralF tag lit) = PatLiteral (f tag) (modifyLitTags f lit)
--     alg (PatIgnoreF tag) = PatIgnore (f tag)

data Expression tag
    = ExprVar (ExprVar tag) Identifier
    | ExprTypeConstructor (ExprTypeConstructor tag) TypeIdentifier
    | ExprApplied (ExprApplied tag) (Expression tag) (Expression tag)
    | ExprTyped (ExprTyped tag) (Expression tag) (Type tag)
    | ExprLet (ExprLet tag) Identifier (Expression tag) (Expression tag)
    | ExprIfElse (ExprIfElse tag) (Expression tag) (Expression tag) (Expression tag)
    | ExprCase (ExprCase tag) (Expression tag) [(Pattern tag, Expression tag)]
    | ExprLambda (ExprLambda tag) [Pattern tag] (Expression tag)
    | ExprLiteral (ExprLiteral tag) (Literal tag)

deriving instance
    ( Show (ExprVar tag)
    , Show (ExprTypeConstructor tag)
    , Show (ExprApplied tag)
    , Show (ExprTyped tag)
    , Show (ExprLet tag)
    , Show (ExprIfElse tag)
    , Show (ExprCase tag)
    , Show (ExprLambda tag)
    , Show (ExprLiteral tag)
    , Show (Pattern tag)
    , Show (Literal tag)
    , Show (Type tag)
    ) =>
    Show (Expression tag)

type family ExprVar tag
type family ExprTypeConstructor tag
type family ExprApplied tag
type family ExprTyped tag
type family ExprLet tag
type family ExprIfElse tag
type family ExprCase tag
type family ExprLambda tag
type family ExprLiteral tag
type instance
    AllTags (Expression tag) =
        '[ ExprVar tag
         , ExprTypeConstructor tag
         , ExprApplied tag
         , ExprTyped tag
         , ExprLet tag
         , ExprIfElse tag
         , ExprCase tag
         , ExprLambda tag
         , ExprLiteral tag
         ]

makeBaseFunctor ''Expression

instance Extract Expression where
    extractTag (ExprVar tag _) = tag
    extractTag (ExprTypeConstructor tag _) = tag
    extractTag (ExprApplied tag _ _) = tag
    extractTag (ExprTyped tag _ _) = tag
    extractTag (ExprLet tag _ _ _) = tag
    extractTag (ExprIfElse tag _ _ _) = tag
    extractTag (ExprCase tag _ _) = tag
    extractTag (ExprLambda tag _ _) = tag
    extractTag (ExprLiteral tag _) = tag

-- modifyExprTags ::
--     forall tag1 tag2.
--     ( AllEq' "Expression" tag1 tag2
--     , AllEq' "Type" tag1 tag2
--     , AllPatEQ tag1 tag2
--     , AllEq '[ExprVar tag1, TypeUnit tag1, PatIgnore tag1]
--     , AllEq '[ExprVar tag2, TypeUnit tag2, PatIgnore tag2]
--     ) =>
--     (ExprVar tag1 -> ExprVar tag2) ->
--     Expression tag1 ->
--     Expression tag2
-- modifyExprTags f = cata alg
--   where
--     alg :: ExpressionF tag1 (Expression tag2) -> Expression tag2
--     alg (ExprVarF tag iden) = ExprVar (f tag) iden
--     alg (ExprTypeConstructorF tag typeIden) = ExprTypeConstructor (f tag) typeIden
--     alg (ExprAppliedF tag expr1 expr2) = ExprApplied (f tag) expr1 expr2
--     alg (ExprTypedF tag expr t) = ExprTyped (f tag) expr (modifyTypeTags f t)
--     alg (ExprLetF tag iden idenExpr expr) = ExprLet (f tag) iden idenExpr expr
--     alg (ExprIfElseF tag condExpr trueExpr falseExpr) = ExprIfElse (f tag) condExpr trueExpr falseExpr
--     alg (ExprCaseF tag matchExpr pats) = ExprCase (f tag) matchExpr $ map (first (modifyPatTags f)) pats
--     alg (ExprLambdaF tag pats expr) = ExprLambda (f tag) (map (modifyPatTags f) pats) expr
--     alg (ExprLiteralF tag lit) = ExprLiteral (f tag) $ modifyLitTags f lit
