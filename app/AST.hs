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

type family ApplyConstraint (xs :: [K.Type]) (c :: K.Type -> K.Constraint) :: K.Constraint where
    ApplyConstraint (x ': xs) c = (c x, ApplyConstraint xs c)
    ApplyConstraint '[] _ = ()

type family ApplyConstraint2 (xs :: [K.Type]) (ys :: [K.Type]) (c :: K.Type -> K.Type -> K.Constraint) :: K.Constraint where
    ApplyConstraint2 (x ': xs) (y ': ys) c = (c x y, ApplyConstraint2 xs ys c)
    ApplyConstraint2 '[] '[] _ = ()

type family ExtractReturn f

type instance ExtractReturn f = TypeOfFirst (AllTags f)
class Extract f where
    extractTag :: (AllEq f) => f -> ExtractReturn f

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
    | TypeUnit (TypeUnit tag)
    | TypeOther (TypeOther tag)

type family TypeArrow tag
type family TypeTuple tag
type family TypeSimple tag
type family TypeUnit tag

type instance AllTags (Type tag) = '[TypeArrow tag, TypeTuple tag, TypeSimple tag, TypeUnit tag]

type family TypeOther tag

makeBaseFunctor ''Type

deriving instance (ApplyConstraint (AllTags (Type tag)) Show, Show (TypeOther tag)) => Show (Type tag)

instance (Extract (TypeOther tag)) => Extract (Type tag) where
    extractTag (TypeArrow tag _ _) = tag
    extractTag (TypeTuple tag _) = tag
    extractTag (TypeSimple tag _) = tag
    extractTag (TypeUnit tag) = tag

data Sign = Positive | Negative
    deriving (Show, Eq)

data Literal tag
    = LitNumber (LitNumber tag) Sign Text
    | LitString (LitString tag) Text
    | LitChar (LitChar tag) Char
    | LitUnit (LitUnit tag)

type family LitNumber tag
type family LitString tag
type family LitChar tag
type family LitUnit tag
type instance AllTags (Literal tag) = '[LitNumber tag, LitString tag, LitChar tag, LitUnit tag]

deriving instance (ApplyConstraint (AllTags (Literal tag)) Show) => Show (Literal tag)

instance Extract (Literal tag) where
    extractTag (LitNumber tag _ _) = tag
    extractTag (LitString tag _) = tag
    extractTag (LitChar tag _) = tag
    extractTag (LitUnit tag) = tag

data Pattern tag
    = PatVariable (PatVariable tag) Identifier
    | PatCapture (PatCapture tag) Identifier (Pattern tag)
    | PatConstructor (PatConstructor tag) TypeIdentifier [Pattern tag]
    | PatLiteral (PatLiteral tag) (Literal tag)
    | PatTuple (PatTuple tag) [Pattern tag]
    | PatIgnore (PatIgnore tag)

type family PatVariable tag
type family PatCapture tag
type family PatConstructor tag
type family PatLiteral tag
type family PatTuple tag
type family PatIgnore tag
type instance
    AllTags (Pattern tag) =
        '[ PatVariable tag
         , PatCapture tag
         , PatConstructor tag
         , PatLiteral tag
         , PatTuple tag
         , PatIgnore tag
         ]

makeBaseFunctor ''Pattern

deriving instance (ApplyConstraint (AllTags (Pattern tag)) Show, Show (Literal tag)) => Show (Pattern tag)

instance Extract (Pattern tag) where
    extractTag (PatVariable tag _) = tag
    extractTag (PatCapture tag _ _) = tag
    extractTag (PatConstructor tag _ _) = tag
    extractTag (PatLiteral tag _) = tag
    extractTag (PatTuple tag _) = tag
    extractTag (PatIgnore tag) = tag

data Expression tag
    = ExprVar (ExprVar tag) Identifier
    | ExprTypeConstructor (ExprTypeConstructor tag) TypeIdentifier
    | ExprApplied (ExprApplied tag) (Expression tag) (Expression tag)
    | ExprTyped (ExprTyped tag) (Expression tag) (Type tag)
    | ExprLet (ExprLet tag) [(Identifier, Expression tag)] (Expression tag)
    | ExprIfElse (ExprIfElse tag) (Expression tag) (Expression tag) (Expression tag)
    | ExprCase (ExprCase tag) (Expression tag) [(Pattern tag, Expression tag)]
    | ExprLambda (ExprLambda tag) [Pattern tag] (Expression tag)
    | ExprLiteral (ExprLiteral tag) (Literal tag)
    | ExprTuple (ExprTuple tag) [Expression tag]

type family ExprVar tag
type family ExprTypeConstructor tag
type family ExprApplied tag
type family ExprTyped tag
type family ExprLet tag
type family ExprIfElse tag
type family ExprCase tag
type family ExprLambda tag
type family ExprLiteral tag
type family ExprTuple tag
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
         , ExprTuple tag
         ]

makeBaseFunctor ''Expression

deriving instance
    ( ApplyConstraint (AllTags (Expression tag)) Show
    , Show (Pattern tag)
    , Show (Literal tag)
    , Show (Type tag)
    ) =>
    Show (Expression tag)

instance Extract (Expression tag) where
    extractTag (ExprVar tag _) = tag
    extractTag (ExprTypeConstructor tag _) = tag
    extractTag (ExprApplied tag _ _) = tag
    extractTag (ExprTyped tag _ _) = tag
    extractTag (ExprLet tag _ _) = tag
    extractTag (ExprIfElse tag _ _ _) = tag
    extractTag (ExprCase tag _ _) = tag
    extractTag (ExprLambda tag _ _) = tag
    extractTag (ExprLiteral tag _) = tag
    extractTag (ExprTuple tag _) = tag

data GADT tag = GADT (GADTtag tag) TypeIdentifier [Constructor tag]

type family GADTtag tag
type instance AllTags (GADT tag) = '[GADTtag tag]

deriving instance (ApplyConstraint (AllTags (GADT tag)) Show, Show (Constructor tag)) => Show (GADT tag)

---```
-- Bar : Int -> (Bool, Int) -> Foo
-- ```
data Constructor tag = Constructor (Constructortag tag) TypeIdentifier (Type tag)

type family Constructortag tag
type instance AllTags (Constructor tag) = '[Constructortag tag]

deriving instance (ApplyConstraint (AllTags (Constructor tag)) Show, Show (Type tag)) => Show (Constructor tag)

-- makes sure the constructor constructs a correct type
mkConstructor :: TypeIdentifier -> TypeIdentifier -> Constructortag tag -> Type tag -> Maybe (Constructor tag)
mkConstructor typeName constructorName tag signature
    | checkSignature signature = Just $ Constructor tag constructorName signature
    | otherwise = Nothing
  where
    checkSignature (TypeArrow _ _ t) = checkSignature t
    checkSignature (TypeTuple _ _) = False
    checkSignature (TypeSimple _ typeIden) = typeIden == typeName
    checkSignature (TypeUnit _) = False

data Function tag = Function (Functiontag tag) Identifier (Type tag) [FunctionVariant tag]

type family Functiontag tag
type instance AllTags (Function tag) = '[Functiontag tag]

deriving instance
    ( ApplyConstraint (AllTags (Function tag)) Show
    , Show (Type tag)
    , Show (FunctionVariant tag)
    ) =>
    Show (Function tag)

data FunctionVariant tag = FunctionVariant (FunctionVarianttag tag) [Pattern tag] (Expression tag)

type family FunctionVarianttag tag
type instance AllTags (FunctionVariant tag) = '[FunctionVarianttag tag]

deriving instance
    ( ApplyConstraint (AllTags (FunctionVariant tag)) Show
    , Show (Expression tag)
    , Show (Pattern tag)
    ) =>
    Show (FunctionVariant tag)
