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
--    HasVar (..),
--    occurs,
  )
where

import Data.Map (Map)
--import Data.Set (Set)
--import Data.Set qualified as Set
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
  = BITyBase
  | BITyArrow (BITypeF bt ann) (BITypeF bt ann)
  | BITyOptArrow (BITypeF bt ann) (BITypeF bt ann)
  deriving stock (Show)

type BIType = BITypeF BindingTime (BindingTime, Span)

data BindingTimeEnvEntry
  = EntryBuiltInPersistent (BITypeF () ())
  | EntryBuiltInFixed Var BindingTimeConst (BITypeF BindingTimeConst BindingTimeConst)
  | EntryLocallyBound BindingTime BIType
  deriving stock (Show)

type BindingTimeEnv = Map Var BindingTimeEnvEntry

type BExpr = ExprF (BindingTime, Span)

type BTypeExpr = TypeExprF (BindingTime, Span)

--class HasVar a where
--  frees :: a -> Set Var
--  subst :: BExpr -> Var -> a -> a
--
--instance HasVar BExpr where
--  frees (Expr _ann exprMain) =
--    case exprMain of
--      Literal _ ->
--        Set.empty
--      Var x ->
--        Set.singleton x
--      Lam Nothing (x, tye1) e2 ->
--        Set.union (frees tye1) (Set.delete x (frees e2))
--      Lam (Just (f, tyeRec)) (x, tye1) e2 ->
--        Set.unions [frees tyeRec, frees tye1, Set.delete f (Set.delete x (frees e2))]
--      App e1 e2 ->
--        Set.union (frees e1) (frees e2)
--      LetIn x e1 e2 ->
--        Set.union (frees e1) (Set.delete x (frees e2))
--      IfThenElse e0 e1 e2 ->
--        Set.unions [frees e0, frees e1, frees e2]
--      As e1 tye2 ->
--        Set.union (frees e1) (frees tye2)
--
--  subst be0 x be@(Expr ann exprMain) =
--    case exprMain of
--      Literal _ ->
--        be
--      Var y ->
--        if y == x then be0 else be
--      Lam Nothing (y, tye1) e2 ->
--        Expr ann (Lam Nothing (y, go tye1) (if y == x then e2 else go e2))
--      Lam (Just (f, tyeRec)) (y, tye1) e2 ->
--        Expr ann (Lam (Just (f, go tyeRec)) (y, go tye1) (if f == x || y == x then e2 else go e2))
--      App e1 e2 ->
--        Expr ann (App (go e1) (go e2))
--      LetIn y e1 e2 ->
--        Expr ann (LetIn y (go e1) (if y == x then e2 else go e2))
--      IfThenElse e0 e1 e2 ->
--        Expr ann (IfThenElse (go e0) (go e1) (go e2))
--      As e1 tye2 ->
--        Expr ann (As (go e1) (go tye2))
--    where
--      go :: forall a. (HasVar a) => a -> a
--      go = subst be0 x
--
--instance HasVar BTypeExpr where
--  frees (TypeExpr _ann typeExprMain) =
--    case typeExprMain of
--      TyName _tyName args -> Set.unions (map frees args)
--      TyArrow (Nothing, tye1) tye2 -> Set.union (frees tye1) (frees tye2)
--      TyArrow (Just y, tye1) tye2 -> Set.union (frees tye1) (Set.delete y (frees tye2))
--
--  subst be0 x (TypeExpr ann typeExprMain) =
--    TypeExpr ann $
--      case typeExprMain of
--        TyName tyName args -> TyName tyName (map go args)
--        TyArrow (Nothing, btye1) btye2 -> TyArrow (Nothing, go btye1) (go btye2)
--        TyArrow (Just y, btye1) btye2 -> TyArrow (Just y, go btye1) (if y == x then btye2 else go btye2)
--    where
--      go :: forall a. (HasVar a) => a -> a
--      go = subst be0 x
--
--occurs :: (HasVar a) => Var -> a -> Bool
--occurs x entity = x `elem` frees entity
