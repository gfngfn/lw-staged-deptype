module Lwsd.Subst
  ( HasVar0 (..),
    occurs0,
    alphaEquivalent,
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import Lwsd.Syntax
import Prelude

-- TODO (refactor): use `Traversal` to implement `occurs0` and `subst0`

class HasVar0 a where
  frees0 :: a -> Set Var
  subst0 :: Ass0Expr -> Var -> a -> a

occurs0 :: (HasVar0 a) => Var -> a -> Bool
occurs0 x e = x `elem` frees0 e

alphaEquivalent :: (Eq a) => a -> a -> Bool
alphaEquivalent = (==) -- TODO: generalize this

instance HasVar0 Ass0Expr where
  frees0 = \case
    A0Literal _ -> Set.empty
    A0AppBuiltIn _ -> Set.empty -- We do not see variables for built-in functions
    A0Var y -> Set.singleton y
    A0Lam (y, a0tye1) a0e2 -> Set.union (frees0 a0tye1) (Set.delete y (frees0 a0e2))
    A0App a0e1 a0e2 -> Set.union (frees0 a0e1) (frees0 a0e2)
    A0IfThenElse a0e0 a0e1 a0e2 -> Set.union (Set.union (frees0 a0e0) (frees0 a0e1)) (frees0 a0e2)
    A0Bracket a1e1 -> frees0 a1e1
    A0TyEqAssert _ ty0eq -> frees0 ty0eq

  subst0 a0e x = \case
    A0Literal lit ->
      A0Literal lit
    A0AppBuiltIn bi ->
      A0AppBuiltIn bi -- We do not see variables for built-in functions
    A0Var y ->
      if y == x then a0e else A0Var y
    A0Lam (y, a0tye1) a0e2 ->
      A0Lam (y, go a0tye1) (if y == x then a0e2 else go a0e2)
    A0App a0e1 a0e2 ->
      A0App (go a0e1) (go a0e2)
    A0IfThenElse a0e0 a0e1 a0e2 ->
      A0IfThenElse (go a0e0) (go a0e1) (go a0e2)
    A0Bracket a1e1 ->
      A0Bracket (go a1e1)
    A0TyEqAssert loc ty0eq ->
      A0TyEqAssert loc (go ty0eq)
    where
      go :: forall a. (HasVar0 a) => a -> a
      go = subst0 a0e x

instance HasVar0 Ass1Expr where
  frees0 = \case
    A1Literal _ -> Set.empty
    A1Var _ -> Set.empty -- Does not collect stage-1 variables
    A1Lam (y, a1tye1) a1e2 -> Set.union (frees0 a1tye1) (Set.delete y (frees0 a1e2))
    A1App a1e1 a1e2 -> Set.union (frees0 a1e1) (frees0 a1e2)
    A1IfThenElse a1e0 a1e1 a1e2 -> Set.union (Set.union (frees0 a1e0) (frees0 a1e1)) (frees0 a1e2)
    A1Escape a0e1 -> frees0 a0e1

  subst0 a0e x = \case
    A1Literal lit ->
      A1Literal lit
    A1Var y ->
      A1Var y -- Does not change since we substitute stage-0 variables.
    A1Lam (y, a1tye1) a1e2 ->
      A1Lam (y, go a1tye1) (if y == x then a1e2 else go a1e2)
    A1App a1e1 a1e2 ->
      A1App (go a1e1) (go a1e2)
    A1IfThenElse a1e0 a1e1 a1e2 ->
      A1IfThenElse (go a1e0) (go a1e1) (go a1e2)
    A1Escape a0e1 ->
      A1Escape (go a0e1)
    where
      go :: forall a. (HasVar0 a) => a -> a
      go = subst0 a0e x

instance HasVar0 Ass0TypeExpr where
  frees0 = \case
    A0TyPrim _ -> Set.empty
    A0TyArrow (yOpt, a0tye1) a0tye2 ->
      Set.union (frees0 a0tye1) $
        case yOpt of
          Nothing -> frees0 a0tye2
          Just y -> Set.delete y (frees0 a0tye2)
    A0TyCode a1tye1 -> frees0 a1tye1

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
  frees0 = \case
    A1TyPrim a1tyPrim ->
      case a1tyPrim of
        A1TyInt -> Set.empty
        A1TyBool -> Set.empty
        A1TyVec a0e1 -> frees0 a0e1
        A1TyMat a0e1 a0e2 -> Set.union (frees0 a0e1) (frees0 a0e2)
    A1TyArrow a1tye1 a1tye2 -> Set.union (frees0 a1tye1) (frees0 a1tye2)

  subst0 a0e x = \case
    A1TyPrim a1tyPrim ->
      A1TyPrim $ case a1tyPrim of
        A1TyInt -> A1TyInt
        A1TyBool -> A1TyBool
        A1TyVec a0e1 -> A1TyVec (go a0e1)
        A1TyMat a0e1 a0e2 -> A1TyMat (go a0e1) (go a0e2)
    A1TyArrow a1tye1 a1tye2 ->
      A1TyArrow (go a1tye1) (go a1tye2)
    where
      go :: forall a. (HasVar0 a) => a -> a
      go = subst0 a0e x

instance HasVar0 Type1Equation where
  frees0 = \case
    TyEq1Prim ty1eqPrim ->
      case ty1eqPrim of
        TyEq1Int -> Set.empty
        TyEq1Bool -> Set.empty
        TyEq1Vec a0e1 a0e2 -> Set.union (frees0 a0e1) (frees0 a0e2)
        TyEq1Mat a0e11 a0e12 a0e21 a0e22 -> Set.unions $ map frees0 [a0e11, a0e12, a0e21, a0e22]
    TyEq1Arrow ty1eqDom ty1eqCod -> Set.union (frees0 ty1eqDom) (frees0 ty1eqCod)

  subst0 a0e x = \case
    TyEq1Prim ty1eqPrim ->
      TyEq1Prim $
        case ty1eqPrim of
          TyEq1Int -> TyEq1Int
          TyEq1Bool -> TyEq1Bool
          TyEq1Vec a0e1 a0e2 -> TyEq1Vec (go a0e1) (go a0e2)
          TyEq1Mat a0e11 a0e12 a0e21 a0e22 -> TyEq1Mat (go a0e11) (go a0e12) (go a0e21) (go a0e22)
    TyEq1Arrow ty1eqDom ty1eqCod -> TyEq1Arrow (go ty1eqDom) (go ty1eqCod)
    where
      go :: forall a. (HasVar0 a) => a -> a
      go = subst0 a0e x
