module Surface.BindingTime.Core
  ( BindingTimeVar (..),
    BindingTimeConst (..),
    BindingTime (..),
    BITypeF (..),
    BITypeMainF (..),
    BIType,
    BindingTimeEnvEntry (..),
    BindingTimeEnv,
    BExpr,
    BTypeExpr,
    HasVar (..),
    occurs,
  )
where

import Data.Map (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics
import Surface.Syntax
import Util.TokenUtil
import Prelude

newtype BindingTimeVar = BindingTimeVar Int
  deriving stock (Eq, Ord, Show)

data BindingTimeConst = BT0 | BT1
  deriving stock (Eq, Ord, Show) -- BT0 < BT1

data BindingTime
  = BTConst BindingTimeConst
  | BTVar BindingTimeVar
  deriving stock (Eq, Show, Generic)

-- Intermediate, minimal type representations for binding-time analysis
data BITypeF bt ann = BIType bt (BITypeMainF bt ann)
  deriving stock (Show)

data BITypeMainF bt ann
  = BITyBase [ExprF ann]
  | BITyArrow (Maybe Var, BITypeF bt ann) (BITypeF bt ann)
  deriving stock (Show)

type BIType = BITypeF BindingTime (BindingTime, Span)

data BindingTimeEnvEntry
  = EntryBuiltInPersistent (BITypeF () ())
  | EntryBuiltInFixed Var BindingTimeConst (BITypeF BindingTimeConst BindingTimeConst)
  | EntryLocallyBound BindingTime BIType

type BindingTimeEnv = Map Var BindingTimeEnvEntry

type BExpr = ExprF (BindingTime, Span)

type BTypeExpr = TypeExprF (BindingTime, Span)

class HasVar a where
  frees :: a -> Set Var
  subst :: BExpr -> Var -> a -> a

instance HasVar BIType where
  frees (BIType _meta bityMain) =
    case bityMain of
      BITyBase bes -> Set.unions (map frees bes)
      BITyArrow (Nothing, btye1) btye2 -> Set.union (frees btye1) (frees btye2)
      BITyArrow (Just x1, btye1) btye2 -> Set.union (frees btye1) (Set.delete x1 (frees btye2))
  subst be0 x (BIType meta bityMain) =
    BIType meta $
      case bityMain of
        BITyBase bes -> BITyBase (map f bes)
        BITyArrow (Nothing, btye1) btye2 -> BITyArrow (Nothing, f btye1) (f btye2)
        BITyArrow (Just y, btye1) btye2 -> BITyArrow (Just y, f btye1) (if y == x then btye2 else f btye2)
    where
      f :: forall a. (HasVar a) => a -> a
      f = subst be0 x

instance HasVar BExpr where
  frees (Expr _ann exprMain) =
    case exprMain of
      Literal _ -> Set.empty
      Var x -> Set.singleton x
      Lam (x, tye1) e2 -> Set.union (frees tye1) (Set.delete x (frees e2))
      App e1 e2 -> Set.union (frees e1) (frees e2)
      LetIn x e1 e2 -> Set.union (frees e1) (Set.delete x (frees e2))
  subst be0 x be@(Expr ann exprMain) =
    case exprMain of
      Literal _ -> be
      Var y -> if y == x then be0 else be
      Lam (y, tye1) e2 -> Expr ann (Lam (y, f tye1) (if y == x then e2 else f e2))
      App e1 e2 -> Expr ann (App (f e1) (f e2))
      LetIn y e1 e2 -> Expr ann (LetIn y (f e1) (if y == x then e2 else f e2))
    where
      f :: forall a. (HasVar a) => a -> a
      f = subst be0 x

instance HasVar BTypeExpr where
  frees (TypeExpr _ann typeExprMain) =
    case typeExprMain of
      TyName _tyName args -> Set.unions (map frees args)
      TyArrow (Nothing, tye1) tye2 -> Set.union (frees tye1) (frees tye2)
      TyArrow (Just y, tye1) tye2 -> Set.union (frees tye1) (Set.delete y (frees tye2))
  subst be0 x (TypeExpr ann typeExprMain) =
    TypeExpr ann $
      case typeExprMain of
        TyName tyName args -> TyName tyName (map f args)
        TyArrow (Nothing, btye1) btye2 -> TyArrow (Nothing, f btye1) (f btye2)
        TyArrow (Just y, btye1) btye2 -> TyArrow (Just y, f btye1) (if y == x then btye2 else f btye2)
    where
      f :: forall a. (HasVar a) => a -> a
      f = subst be0 x

occurs :: (HasVar a) => Var -> a -> Bool
occurs x entity = x `elem` frees entity
