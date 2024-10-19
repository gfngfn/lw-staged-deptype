module Lwsd.Subst
  ( HasVar (..),
    frees0,
    subst0,
    subst1,
    occurs0,
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import Lwsd.Syntax
import Prelude

-- TODO (refactor): use `Traversal` to implement `occurs0` and `subst0`

data Subst
  = Subst0 Var Ass0Expr
  | Subst1 Var Ass1Expr

class HasVar a where
  frees :: a -> (Set Var, Set Var)
  subst :: Subst -> a -> a
  alphaEquivalent :: a -> a -> Bool

frees0 :: (HasVar a) => a -> Set Var
frees0 = fst . frees

subst0 :: (HasVar a) => Ass0Expr -> Var -> a -> a
subst0 a0e x = subst (Subst0 x a0e)

subst1 :: (HasVar a) => Ass1Expr -> Var -> a -> a
subst1 a1e x = subst (Subst1 x a1e)

occurs0 :: (HasVar a) => Var -> a -> Bool
occurs0 x e = x `elem` frees0 e

unionPairs :: [(Set Var, Set Var)] -> (Set Var, Set Var)
unionPairs pairs =
  (Set.unions (map fst pairs), Set.unions (map snd pairs))

instance HasVar Ass0Expr where
  frees = \case
    A0Literal _ ->
      (Set.empty, Set.empty)
    A0AppBuiltIn _ ->
      (Set.empty, Set.empty) -- We do not see variables for built-in functions
    A0Var y ->
      (Set.singleton y, Set.empty)
    A0Lam Nothing (y, a0tye1) a0e2 ->
      let (var0set1, var1set1) = frees a0tye1
          (var0set2, var1set2) = frees a0e2
       in (Set.union var0set1 (Set.delete y var0set2), Set.union var1set1 var1set2)
    A0Lam (Just (f, a0tyeRec)) (y, a0tye1) a0e2 ->
      let (var0setRec, var1setRec) = frees a0tyeRec
          (var0set1, var1set1) = frees a0tye1
          (var0set2, var1set2) = frees a0e2
          var0set = Set.unions [var0setRec, var0set1, Set.delete f (Set.delete y var0set2)]
          var1set = Set.unions [var1setRec, var1set1, var1set2]
       in (var0set, var1set)
    A0App a0e1 a0e2 ->
      unionPairs [frees a0e1, frees a0e2]
    A0IfThenElse a0e0 a0e1 a0e2 ->
      unionPairs [frees a0e0, frees a0e1, frees a0e2]
    A0Bracket a1e1 ->
      frees a1e1
    A0TyEqAssert _ ty0eq ->
      frees ty0eq

  subst s = \case
    A0Literal lit ->
      A0Literal lit
    A0AppBuiltIn bi ->
      A0AppBuiltIn bi -- We do not see variables for built-in functions
    A0Var y ->
      case s of
        Subst0 x a0e -> if y == x then a0e else A0Var y
        Subst1 _ _ -> A0Var y
    A0Lam Nothing (y, a0tye1) a0e2 ->
      A0Lam Nothing (y, go a0tye1) $
        case s of
          Subst0 x _ -> if y == x then a0e2 else go a0e2
          Subst1 _ _ -> go a0e2
    A0Lam (Just (f, a0tyeRec)) (y, a0tye1) a0e2 ->
      A0Lam (Just (f, go a0tyeRec)) (y, go a0tye1) $
        case s of
          Subst0 x _ -> if f == x || y == x then a0e2 else go a0e2
          Subst1 _ _ -> go a0e2
    A0App a0e1 a0e2 ->
      A0App (go a0e1) (go a0e2)
    A0IfThenElse a0e0 a0e1 a0e2 ->
      A0IfThenElse (go a0e0) (go a0e1) (go a0e2)
    A0Bracket a1e1 ->
      A0Bracket (go a1e1)
    A0TyEqAssert loc ty0eq ->
      A0TyEqAssert loc (go ty0eq)
    where
      go :: forall a. (HasVar a) => a -> a
      go = subst s

  alphaEquivalent a0e1 a0e2 =
    case (a0e1, a0e2) of
      (A0Literal lit1, A0Literal lit2) ->
        lit1 == lit2
      (A0AppBuiltIn builtIn1, A0AppBuiltIn builtIn2) ->
        case (builtIn1, builtIn2) of
          (BIAssertNat _loc1 x1, BIAssertNat _loc2 x2) -> x1 == x2 -- Ignores the difference of `Span`
          (_, _) -> builtIn1 == builtIn2
      (A0Var x1, A0Var x2) ->
        x1 == x2
      (A0Lam Nothing (x1, a0tye11) a0e12, A0Lam Nothing (x2, a0tye21) a0e22) ->
        alphaEquivalent a0tye11 a0tye21
          && alphaEquivalent a0e12 (subst0 (A0Var x1) x2 a0e22)
      (A0Lam (Just (f1, a0tye1Rec)) (x1, a0tye11) a0e12, A0Lam (Just (f2, a0tye2Rec)) (x2, a0tye21) a0e22) ->
        alphaEquivalent a0tye1Rec a0tye2Rec
          && alphaEquivalent a0tye11 a0tye21
          && alphaEquivalent a0e12 (subst0 (A0Var f1) f2 (subst0 (A0Var x1) x2 a0e22))
      (A0App a0e11 a0e12, A0App a0e21 a0e22) ->
        alphaEquivalent a0e11 a0e21
          && alphaEquivalent a0e12 a0e22
      (A0IfThenElse a0e10 a0e11 a0e12, A0IfThenElse a0e20 a0e21 a0e22) ->
        alphaEquivalent a0e10 a0e20
          && alphaEquivalent a0e11 a0e21
          && alphaEquivalent a0e12 a0e22
      (A0Bracket a1e1, A0Bracket a1e2) ->
        alphaEquivalent a1e1 a1e2
      (A0TyEqAssert _ ty0eq1, A0TyEqAssert _ ty0eq2) ->
        alphaEquivalent ty0eq1 ty0eq2
      (_, _) ->
        False

instance HasVar Ass1Expr where
  frees = \case
    A1Literal _ ->
      (Set.empty, Set.empty)
    A1Var y ->
      (Set.empty, Set.singleton y)
    A1Lam Nothing (y, a1tye1) a1e2 ->
      let (var0set1, var1set1) = frees a1tye1
          (var0set2, var1set2) = frees a1e2
       in (Set.union var0set1 var0set2, Set.union var1set1 (Set.delete y var1set2))
    A1Lam (Just (f, a1tyeRec)) (y, a1tye1) a1e2 ->
      let (var0setRec, var1setRec) = frees a1tyeRec
          (var0set1, var1set1) = frees a1tye1
          (var0set2, var1set2) = frees a1e2
          var0set = Set.unions [var0setRec, var0set1, var0set2]
          var1set = Set.unions [var1setRec, var1set1, Set.delete f (Set.delete y var1set2)]
       in (var0set, var1set)
    A1App a1e1 a1e2 ->
      unionPairs [frees a1e1, frees a1e2]
    A1IfThenElse a1e0 a1e1 a1e2 ->
      unionPairs [frees a1e0, frees a1e1, frees a1e2]
    A1Escape a0e1 ->
      frees a0e1

  subst s = \case
    A1Literal lit ->
      A1Literal lit
    A1Var y ->
      case s of
        Subst0 _ _ -> A1Var y
        Subst1 x a1e -> if y == x then a1e else A1Var y
    A1Lam Nothing (y, a1tye1) a1e2 ->
      A1Lam Nothing (y, go a1tye1) $
        case s of
          Subst0 _ _ -> go a1e2
          Subst1 x _ -> if y == x then a1e2 else go a1e2
    A1Lam (Just (f, a1tyeRec)) (y, a1tye1) a1e2 ->
      A1Lam (Just (f, go a1tyeRec)) (y, go a1tye1) $
        case s of
          Subst0 x _ -> if y == f || y == x then a1e2 else go a1e2
          Subst1 _ _ -> go a1e2
    A1App a1e1 a1e2 ->
      A1App (go a1e1) (go a1e2)
    A1IfThenElse a1e0 a1e1 a1e2 ->
      A1IfThenElse (go a1e0) (go a1e1) (go a1e2)
    A1Escape a0e1 ->
      A1Escape (go a0e1)
    where
      go :: forall a. (HasVar a) => a -> a
      go = subst s

  alphaEquivalent a1e1 a1e2 =
    case (a1e1, a1e2) of
      (A1Literal lit1, A1Literal lit2) ->
        lit1 == lit2
      (A1Var x1, A1Var x2) ->
        x1 == x2
      (A1Lam Nothing (x1, a1tye11) a1e12, A1Lam Nothing (x2, a1tye21) a1e22) ->
        alphaEquivalent a1tye11 a1tye21
          && alphaEquivalent a1e12 (subst1 (A1Var x1) x2 a1e22)
      (A1Lam (Just (f1, a1tye1Rec)) (x1, a1tye11) a1e12, A1Lam (Just (f2, a1tye2Rec)) (x2, a1tye21) a1e22) ->
        alphaEquivalent a1tye1Rec a1tye2Rec
          && alphaEquivalent a1tye11 a1tye21
          && alphaEquivalent a1e12 (subst1 (A1Var x1) x2 (subst1 (A1Var f1) f2 a1e22))
      (A1App a1e11 a1e12, A1App a1e21 a1e22) ->
        alphaEquivalent a1e11 a1e21
          && alphaEquivalent a1e12 a1e22
      (A1IfThenElse a1e10 a1e11 a1e12, A1IfThenElse a1e20 a1e21 a1e22) ->
        alphaEquivalent a1e10 a1e20
          && alphaEquivalent a1e11 a1e21
          && alphaEquivalent a1e12 a1e22
      (A1Escape a0e1, A1Escape a0e2) ->
        alphaEquivalent a0e1 a0e2
      (_, _) ->
        False

instance HasVar Ass0TypeExpr where
  frees = \case
    A0TyPrim _ ->
      (Set.empty, Set.empty)
    A0TyArrow (yOpt, a0tye1) a0tye2 ->
      let (var0set1, var1set1) = frees a0tye1
          (var0set2, var1set2) = frees a0tye2
          var0set =
            Set.union var0set1 $
              case yOpt of
                Nothing -> var0set2
                Just y -> Set.delete y var0set2
          var1set = Set.union var1set1 var1set2
       in (var0set, var1set)
    A0TyCode a1tye1 ->
      frees a1tye1

  subst s = \case
    A0TyPrim a0tyPrim ->
      A0TyPrim a0tyPrim
    A0TyArrow (yOpt, a0tye1) a0tye2 ->
      A0TyArrow (yOpt, go a0tye1) $
        case (yOpt, s) of
          (Just y, Subst0 x _) -> if y == x then a0tye2 else go a0tye2
          (Nothing, _) -> go a0tye2
          (_, Subst1 _ _) -> go a0tye2
    A0TyCode a1tye1 ->
      A0TyCode (go a1tye1)
    where
      go :: forall a. (HasVar a) => a -> a
      go = subst s

  alphaEquivalent a0tye1 a0tye2 =
    case (a0tye1, a0tye2) of
      (A0TyPrim a0tyPrim1, A0TyPrim a0tyPrim2) ->
        a0tyPrim1 == a0tyPrim2 -- Exact match
      (A0TyArrow (y1opt, a0tye11) a0tye12, A0TyArrow (y2opt, a0tye21) a0tye22) ->
        (alphaEquivalent a0tye11 a0tye21 &&) $
          case (y1opt, y2opt) of
            (Nothing, Nothing) ->
              alphaEquivalent a0tye12 a0tye22
            (Just y1, Nothing) ->
              not (occurs0 y1 a0tye12) && alphaEquivalent a0tye12 a0tye22
            (Nothing, Just y2) ->
              not (occurs0 y2 a0tye22) && alphaEquivalent a0tye12 a0tye22
            (Just y1, Just y2) ->
              alphaEquivalent a0tye12 (subst0 (A0Var y1) y2 a0tye22)
      (A0TyCode a1tye1, A0TyCode a1tye2) ->
        alphaEquivalent a1tye1 a1tye2
      (_, _) ->
        False

instance HasVar Ass1TypeExpr where
  frees = \case
    A1TyPrim a1tyPrim ->
      case a1tyPrim of
        A1TyInt -> (Set.empty, Set.empty)
        A1TyBool -> (Set.empty, Set.empty)
        A1TyVec a0e1 -> frees a0e1
        A1TyMat a0e1 a0e2 -> unionPairs [frees a0e1, frees a0e2]
    A1TyArrow a1tye1 a1tye2 ->
      unionPairs [frees a1tye1, frees a1tye2]

  subst s = \case
    A1TyPrim a1tyPrim ->
      A1TyPrim $ case a1tyPrim of
        A1TyInt -> A1TyInt
        A1TyBool -> A1TyBool
        A1TyVec a0e1 -> A1TyVec (go a0e1)
        A1TyMat a0e1 a0e2 -> A1TyMat (go a0e1) (go a0e2)
    A1TyArrow a1tye1 a1tye2 ->
      A1TyArrow (go a1tye1) (go a1tye2)
    where
      go :: forall a. (HasVar a) => a -> a
      go = subst s

  alphaEquivalent a1tye1 a1tye2 =
    case (a1tye1, a1tye2) of
      (A1TyPrim a1tyPrim1, A1TyPrim a1tyPrim2) ->
        case (a1tyPrim1, a1tyPrim2) of
          (A1TyInt, A1TyInt) ->
            True
          (A1TyBool, A1TyBool) ->
            True
          (A1TyVec a0e1, A1TyVec a0e2) ->
            alphaEquivalent a0e1 a0e2
          (A1TyMat a0e11 a0e12, A1TyMat a0e21 a0e22) ->
            alphaEquivalent a0e11 a0e21 && alphaEquivalent a0e12 a0e22
          (_, _) ->
            False
      (A1TyArrow a1tye11 a1tye12, A1TyArrow a1tye21 a1tye22) ->
        alphaEquivalent a1tye11 a1tye21 && alphaEquivalent a1tye12 a1tye22
      (_, _) ->
        False

instance HasVar Type1Equation where
  frees = \case
    TyEq1Prim ty1eqPrim ->
      case ty1eqPrim of
        TyEq1Int -> (Set.empty, Set.empty)
        TyEq1Bool -> (Set.empty, Set.empty)
        TyEq1Vec a0e1 a0e2 -> unionPairs [frees a0e1, frees a0e2]
        TyEq1Mat a0e11 a0e12 a0e21 a0e22 -> unionPairs $ map frees [a0e11, a0e12, a0e21, a0e22]
    TyEq1Arrow ty1eqDom ty1eqCod ->
      unionPairs [frees ty1eqDom, frees ty1eqCod]

  subst s = \case
    TyEq1Prim ty1eqPrim ->
      TyEq1Prim $
        case ty1eqPrim of
          TyEq1Int -> TyEq1Int
          TyEq1Bool -> TyEq1Bool
          TyEq1Vec a0e1 a0e2 -> TyEq1Vec (go a0e1) (go a0e2)
          TyEq1Mat a0e11 a0e12 a0e21 a0e22 -> TyEq1Mat (go a0e11) (go a0e12) (go a0e21) (go a0e22)
    TyEq1Arrow ty1eqDom ty1eqCod ->
      TyEq1Arrow (go ty1eqDom) (go ty1eqCod)
    where
      go :: forall a. (HasVar a) => a -> a
      go = subst s

  alphaEquivalent ty1eq1 ty1eq2 =
    case (ty1eq1, ty1eq2) of
      (TyEq1Prim ty1eqPrim1, TyEq1Prim ty1eqPrim2) ->
        case (ty1eqPrim1, ty1eqPrim2) of
          (TyEq1Int, TyEq1Int) ->
            True
          (TyEq1Bool, TyEq1Bool) ->
            True
          (TyEq1Vec a0e11 a0e12, TyEq1Vec a0e21 a0e22) ->
            alphaEquivalent a0e11 a0e21
              && alphaEquivalent a0e12 a0e22
          (TyEq1Mat a0e111 a0e112 a0e121 a0e122, TyEq1Mat a0e211 a0e212 a0e221 a0e222) ->
            alphaEquivalent a0e111 a0e211
              && alphaEquivalent a0e112 a0e212
              && alphaEquivalent a0e121 a0e221
              && alphaEquivalent a0e122 a0e222
          (_, _) ->
            False
      (TyEq1Arrow ty1eqDom1 ty1eqCod1, TyEq1Arrow ty1eqDom2 ty1eqCod2) ->
        alphaEquivalent ty1eqDom1 ty1eqDom2
          && alphaEquivalent ty1eqCod1 ty1eqCod2
      (_, _) ->
        False
