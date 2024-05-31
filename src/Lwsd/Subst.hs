module Lwsd.Subst where

import Lwsd.Syntax

substExpr0 :: Ass0Expr -> Var -> Ass0Expr -> Ass0Expr
substExpr0 a0e x = \case
  A0Literal lit ->
    A0Literal lit
  A0AppBuiltIn bi ->
    A0AppBuiltIn bi
  A0Var y ->
    if y == x then a0e else A0Var y
  A0Lam (y, a0tye1) a0e2 ->
    A0Lam (y, goTy0 a0tye1) (if y == x then a0e2 else go0 a0e2)
  A0App a0e1 a0e2 ->
    A0App (go0 a0e1) (go0 a0e2)
  A0Bracket a1e1 ->
    A0Bracket (go1 a1e1)
  A0TyEqAssert loc ty0eq a0e0 ->
    A0TyEqAssert loc (goTyEq0 ty0eq) (go0 a0e0)
  where
    go0 = substExpr0 a0e x
    go1 = substExpr1 a0e x
    goTy0 = substTypeExpr0 a0e x
    goTyEq0 = substType0Equality a0e x

substExpr1 :: Ass0Expr -> Var -> Ass1Expr -> Ass1Expr
substExpr1 a0e x = \case
  A1Literal lit ->
    A1Literal lit
  A1Var y ->
    A1Var y -- Does not change since we substitute stage-0 variables.
  A1Lam (y, a1tye1) a1e2 ->
    let a1tye1' = substTypeExpr1 a0e x a1tye1
     in A1Lam (y, a1tye1') (if y == x then a1e2 else go1 a1e2)
  A1App a1e1 a1e2 ->
    A1App (go1 a1e1) (go1 a1e2)
  A1Escape a0e1 ->
    A1Escape (go0 a0e1)
  where
    go0 = substExpr0 a0e x
    go1 = substExpr1 a0e x

substTypeExpr0 :: Ass0Expr -> Var -> Ass0TypeExpr -> Ass0TypeExpr
substTypeExpr0 a0e x = \case
  A0TyPrim a0tyPrim ->
    A0TyPrim a0tyPrim
  A0TyArrow (yOpt, a0tye1) a0tye2 ->
    case yOpt of
      Nothing -> A0TyArrow (Nothing, goTy0 a0tye1) (goTy0 a0tye2)
      Just y -> A0TyArrow (Just y, goTy0 a0tye1) (if y == x then a0tye2 else goTy0 a0tye2)
  A0TyCode a1tye1 ->
    A0TyCode (substTypeExpr1 a0e x a1tye1)
  where
    goTy0 = substTypeExpr0 a0e x

substTypeExpr1 :: Ass0Expr -> Var -> Ass1TypeExpr -> Ass1TypeExpr
substTypeExpr1 a0e x = \case
  A1TyPrim a1tyPrim ->
    A1TyPrim $ case a1tyPrim of
      A1TyInt -> A1TyInt
      A1TyBool -> A1TyBool
      A1TyVec a0e1 -> A1TyVec (go0 a0e1)
  A1TyArrow a1tye1 a1tye2 ->
    A1TyArrow (goTy1 a1tye1) (goTy1 a1tye2)
  where
    go0 = substExpr0 a0e x
    goTy1 = substTypeExpr1 a0e x

substType0Equality :: Ass0Expr -> Var -> Type0Equality -> Type0Equality
substType0Equality a0e x = \case
  TyEq0PrimInt -> TyEq0PrimInt
  TyEq0PrimBool -> TyEq0PrimBool
  TyEq0PrimVec n -> TyEq0PrimVec n
  TyEq0Code ty1eq -> TyEq0Code (goTyEq1 ty1eq)
  TyEq0Arrow yOpt ty0eqDom ty0eqCod ->
    case yOpt of
      Just y | y == x -> TyEq0Arrow (Just y) (goTyEq0 ty0eqDom) ty0eqCod
      _ -> TyEq0Arrow yOpt (goTyEq0 ty0eqDom) (goTyEq0 ty0eqCod)
  where
    goTyEq0 = substType0Equality a0e x
    goTyEq1 = substType1Equality a0e x

substType1Equality :: Ass0Expr -> Var -> Type1Equality -> Type1Equality
substType1Equality a0e x = \case
  TyEq1PrimInt -> TyEq1PrimInt
  TyEq1PrimBool -> TyEq1PrimBool
  TyEq1PrimVec a0e1 a0e2 -> TyEq1PrimVec (go0 a0e1) (go0 a0e2)
  TyEq1Arrow ty1eqDom ty1eqCod -> TyEq1Arrow (goTyEq1 ty1eqDom) (goTyEq1 ty1eqCod)
  where
    go0 = substExpr0 a0e x
    goTyEq1 = substType1Equality a0e x
