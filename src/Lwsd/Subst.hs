module Lwsd.Subst
  ( HasVar0 (..),
  )
where

import Lwsd.Syntax

class HasVar0 a where
  subst0 :: Ass0Expr -> Var -> a -> a

instance HasVar0 Ass0Expr where
  subst0 a0e x = \case
    A0Literal lit ->
      A0Literal lit
    A0AppBuiltIn bi ->
      A0AppBuiltIn bi
    A0Var y ->
      if y == x then a0e else A0Var y
    A0Lam (y, a0tye1) a0e2 ->
      A0Lam (y, go a0tye1) (if y == x then a0e2 else go a0e2)
    A0App a0e1 a0e2 ->
      A0App (go a0e1) (go a0e2)
    A0Bracket a1e1 ->
      A0Bracket (go a1e1)
    A0TyEqAssert loc ty0eq a0e0 ->
      A0TyEqAssert loc (go ty0eq) (go a0e0)
    where
      go :: forall a. (HasVar0 a) => a -> a
      go = subst0 a0e x

instance HasVar0 Ass1Expr where
  subst0 a0e x = \case
    A1Literal lit ->
      A1Literal lit
    A1Var y ->
      A1Var y -- Does not change since we substitute stage-0 variables.
    A1Lam (y, a1tye1) a1e2 ->
      A1Lam (y, go a1tye1) (if y == x then a1e2 else go a1e2)
    A1App a1e1 a1e2 ->
      A1App (go a1e1) (go a1e2)
    A1Escape a0e1 ->
      A1Escape (go a0e1)
    where
      go :: forall a. (HasVar0 a) => a -> a
      go = subst0 a0e x

instance HasVar0 Ass0TypeExpr where
  subst0 a0e x = \case
    A0TyPrim a0tyPrim ->
      A0TyPrim a0tyPrim
    A0TyArrow (yOpt, a0tye1) a0tye2 ->
      case yOpt of
        Nothing -> A0TyArrow (Nothing, go a0tye1) (go a0tye2)
        Just y -> A0TyArrow (Just y, go a0tye1) (if y == x then a0tye2 else go a0tye2)
    A0TyCode a1tye1 ->
      A0TyCode (go a1tye1)
    where
      go :: forall a. (HasVar0 a) => a -> a
      go = subst0 a0e x

instance HasVar0 Ass1TypeExpr where
  subst0 a0e x = \case
    A1TyPrim a1tyPrim ->
      A1TyPrim $ case a1tyPrim of
        A1TyInt -> A1TyInt
        A1TyBool -> A1TyBool
        A1TyVec a0e1 -> A1TyVec (go a0e1)
    A1TyArrow a1tye1 a1tye2 ->
      A1TyArrow (go a1tye1) (go a1tye2)
    where
      go :: forall a. (HasVar0 a) => a -> a
      go = subst0 a0e x

instance HasVar0 Type0Equality where
  subst0 a0e x = \case
    TyEq0PrimInt -> TyEq0PrimInt
    TyEq0PrimBool -> TyEq0PrimBool
    TyEq0PrimVec n -> TyEq0PrimVec n
    TyEq0Code ty1eq -> TyEq0Code (go ty1eq)
    TyEq0Arrow yOpt ty0eqDom ty0eqCod ->
      case yOpt of
        Just y | y == x -> TyEq0Arrow (Just y) (go ty0eqDom) ty0eqCod
        _ -> TyEq0Arrow yOpt (go ty0eqDom) (go ty0eqCod)
    where
      go :: forall a. (HasVar0 a) => a -> a
      go = subst0 a0e x

instance HasVar0 Type1Equality where
  subst0 a0e x = \case
    TyEq1PrimInt -> TyEq1PrimInt
    TyEq1PrimBool -> TyEq1PrimBool
    TyEq1PrimVec a0e1 a0e2 -> TyEq1PrimVec (go a0e1) (go a0e2)
    TyEq1Arrow ty1eqDom ty1eqCod -> TyEq1Arrow (go ty1eqDom) (go ty1eqCod)
    where
      go :: forall a. (HasVar0 a) => a -> a
      go = subst0 a0e x
