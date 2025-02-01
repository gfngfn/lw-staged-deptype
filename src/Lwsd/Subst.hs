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
import Data.Tuple.Extra
import Lwsd.Syntax
import Safe.Exact
import Prelude

-- TODO (refactor): use `Traversal` to implement `occurs0` and `subst0`

data Subst
  = Subst0 AssVar Ass0Expr
  | Subst1 AssVar Ass1Expr

class HasVar a where
  frees :: a -> (Set AssVar, Set AssVar)
  subst :: Subst -> a -> a
  alphaEquivalent :: a -> a -> Bool

frees0 :: (HasVar a) => a -> Set AssVar
frees0 = fst . frees

subst0 :: (HasVar a) => Ass0Expr -> AssVar -> a -> a
subst0 a0e x = subst (Subst0 x a0e)

subst1 :: (HasVar a) => Ass1Expr -> AssVar -> a -> a
subst1 a1e x = subst (Subst1 x a1e)

occurs0 :: (HasVar a) => AssVar -> a -> Bool
occurs0 x e = x `elem` frees0 e

unionPairs :: [(Set AssVar, Set AssVar)] -> (Set AssVar, Set AssVar)
unionPairs pairs =
  (Set.unions (map fst pairs), Set.unions (map snd pairs))

instance (HasVar e) => HasVar (AssLiteral e) where
  frees = \case
    ALitInt _ -> (Set.empty, Set.empty)
    ALitFloat _ -> (Set.empty, Set.empty)
    ALitBool _ -> (Set.empty, Set.empty)
    ALitUnit -> (Set.empty, Set.empty)
    ALitList es -> unionPairs (map frees es)
    ALitVec _ -> (Set.empty, Set.empty)
    ALitMat _ -> (Set.empty, Set.empty)

  subst s = \case
    ALitInt n -> ALitInt n
    ALitFloat r -> ALitFloat r
    ALitBool b -> ALitBool b
    ALitUnit -> ALitUnit
    ALitList es -> ALitList (map (subst s) es)
    ALitVec vec -> ALitVec vec
    ALitMat mat -> ALitMat mat

  alphaEquivalent alit1 alit2 =
    case (alit1, alit2) of
      (ALitInt n1, ALitInt n2) -> n1 == n2
      (ALitBool b1, ALitBool b2) -> b1 == b2
      (ALitList es1, ALitList es2) ->
        case zipExactMay es1 es2 of
          Nothing -> False
          Just zipped -> all (uncurry alphaEquivalent) zipped
      (ALitVec vec1, ALitVec vec2) -> vec1 == vec2
      (ALitMat mat1, ALitMat mat2) -> mat1 == mat2
      (_, _) -> False

instance HasVar Ass0Expr where
  frees = \case
    A0Literal alit ->
      frees alit
    A0AppBuiltIn _ ->
      (Set.empty, Set.empty) -- We do not see variables for built-in functions
    A0Var y ->
      (Set.singleton y, Set.empty)
    A0BuiltInName _ ->
      (Set.empty, Set.empty)
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
    A0LetIn (y, a0tye1) a0e1 a0e2 ->
      frees (A0App (A0Lam Nothing (y, a0tye1) a0e2) a0e1)
    A0Sequential a0e1 a0e2 ->
      unionPairs [frees a0e1, frees a0e2]
    A0IfThenElse a0e0 a0e1 a0e2 ->
      unionPairs [frees a0e0, frees a0e1, frees a0e2]
    A0Bracket a1e1 ->
      frees a1e1
    A0TyEqAssert _ ty0eq ->
      frees ty0eq
    A0RefinementAssert _ a0ePred a0eTarget ->
      unionPairs [frees a0ePred, frees a0eTarget]

  subst s = \case
    A0Literal alit ->
      A0Literal (go alit)
    A0AppBuiltIn bi ->
      A0AppBuiltIn bi -- We do not see variables for built-in functions
    A0Var y ->
      case s of
        Subst0 x a0e -> if y == x then a0e else A0Var y
        Subst1 _ _ -> A0Var y
    A0BuiltInName builtInName ->
      A0BuiltInName builtInName
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
    A0LetIn (y, a0tye1) a0e1 a0e2 ->
      A0LetIn (y, go a0tye1) (go a0e1) $
        case s of
          Subst0 x _ -> if y == x then a0e2 else go a0e2
          Subst1 _ _ -> go a0e2
    A0Sequential a0e1 a0e2 ->
      A0Sequential (go a0e1) (go a0e2)
    A0IfThenElse a0e0 a0e1 a0e2 ->
      A0IfThenElse (go a0e0) (go a0e1) (go a0e2)
    A0Bracket a1e1 ->
      A0Bracket (go a1e1)
    A0TyEqAssert loc ty0eq ->
      A0TyEqAssert loc (go ty0eq)
    A0RefinementAssert loc a0ePred a0eTarget ->
      A0RefinementAssert loc (go a0ePred) (go a0eTarget)
    where
      go :: forall a. (HasVar a) => a -> a
      go = subst s

  alphaEquivalent a0e1 a0e2 =
    case (a0e1, a0e2) of
      (A0Literal alit1, A0Literal alit2) ->
        alphaEquivalent alit1 alit2
      (A0AppBuiltIn builtIn1, A0AppBuiltIn builtIn2) ->
        builtIn1 == builtIn2
      (A0Var x1, A0Var x2) ->
        x1 == x2
      (A0BuiltInName builtInName1, A0BuiltInName builtInName2) ->
        builtInName1 == builtInName2
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
      (A0LetIn (x1, a0tye1) a0e11 a0e12, A0LetIn (x2, a0tye2) a0e21 a0e22) ->
        alphaEquivalent a0tye1 a0tye2
          && alphaEquivalent a0e11 a0e21
          && alphaEquivalent a0e12 (subst0 (A0Var x1) x2 a0e22)
      (A0Sequential a0e11 a0e12, A0Sequential a0e21 a0e22) ->
        alphaEquivalent a0e11 a0e21 && alphaEquivalent a0e12 a0e22
      (A0IfThenElse a0e10 a0e11 a0e12, A0IfThenElse a0e20 a0e21 a0e22) ->
        alphaEquivalent a0e10 a0e20
          && alphaEquivalent a0e11 a0e21
          && alphaEquivalent a0e12 a0e22
      (A0Bracket a1e1, A0Bracket a1e2) ->
        alphaEquivalent a1e1 a1e2
      (A0TyEqAssert _ ty0eq1, A0TyEqAssert _ ty0eq2) ->
        alphaEquivalent ty0eq1 ty0eq2
      (A0RefinementAssert _ a0ePred1 a0eTarget1, A0RefinementAssert _ a0ePred2 a0eTarget2) ->
        alphaEquivalent a0ePred1 a0ePred2 && alphaEquivalent a0eTarget1 a0eTarget2
      (_, _) ->
        False

instance HasVar Ass1Expr where
  frees = \case
    A1Literal alit ->
      frees alit
    A1Var y ->
      (Set.empty, Set.singleton y)
    A1BuiltInName _ ->
      (Set.empty, Set.empty)
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
    A1Sequential a1e1 a1e2 ->
      unionPairs [frees a1e1, frees a1e2]
    A1IfThenElse a1e0 a1e1 a1e2 ->
      unionPairs [frees a1e0, frees a1e1, frees a1e2]
    A1Escape a0e1 ->
      frees a0e1

  subst s = \case
    A1Literal alit ->
      A1Literal (go alit)
    A1Var y ->
      case s of
        Subst0 _ _ -> A1Var y
        Subst1 x a1e -> if y == x then a1e else A1Var y
    A1BuiltInName a1builtInName ->
      A1BuiltInName a1builtInName
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
    A1Sequential a1e1 a1e2 ->
      A1Sequential (go a1e1) (go a1e2)
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
        alphaEquivalent lit1 lit2
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
    A0TyPrim _ maybePred ->
      frees maybePred
    A0TyList a0tye maybePred ->
      unionPairs [frees a0tye, frees maybePred]
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
    A0TyOptArrow (y, a0tye1) a0tye2 ->
      let (var0set1, var1set1) = frees a0tye1
          (var0set2, var1set2) = frees a0tye2
          var0set = Set.union var0set1 (Set.delete y var0set2)
          var1set = Set.union var1set1 var1set2
       in (var0set, var1set)

  subst s = \case
    A0TyPrim a0tyPrim maybePred ->
      A0TyPrim a0tyPrim (go maybePred)
    A0TyList a0tye maybePred ->
      A0TyList (go a0tye) (go maybePred)
    A0TyArrow (yOpt, a0tye1) a0tye2 ->
      A0TyArrow (yOpt, go a0tye1) $
        case (yOpt, s) of
          (Just y, Subst0 x _) -> if y == x then a0tye2 else go a0tye2
          (Nothing, _) -> go a0tye2
          (_, Subst1 _ _) -> go a0tye2
    A0TyCode a1tye1 ->
      A0TyCode (go a1tye1)
    A0TyOptArrow (y, a0tye1) a0tye2 ->
      case s of
        Subst0 x _ -> A0TyOptArrow (y, go a0tye1) (if y == x then a0tye2 else go a0tye2)
        Subst1 _ _ -> A0TyOptArrow (y, go a0tye1) (go a0tye2)
    where
      go :: forall a. (HasVar a) => a -> a
      go = subst s

  alphaEquivalent a0tye1 a0tye2 =
    case (a0tye1, a0tye2) of
      (A0TyPrim a0tyPrim1 maybePred1, A0TyPrim a0tyPrim2 maybePred2) ->
        -- Exact match
        a0tyPrim1 == a0tyPrim2 && alphaEquivalent maybePred1 maybePred2
      (A0TyList a0tye1' maybePred1, A0TyList a0tye2' maybePred2) ->
        alphaEquivalent a0tye1' a0tye2' && alphaEquivalent maybePred1 maybePred2
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
      (A0TyOptArrow (y1, a0tye11) a0tye12, A0TyOptArrow (y2, a0tye21) a0tye22) ->
        alphaEquivalent a0tye11 a0tye21 && alphaEquivalent a0tye12 (subst0 (A0Var y1) y2 a0tye22)
      (_, _) ->
        False

instance HasVar Ass1TypeExpr where
  frees = \case
    A1TyPrim a1tyPrim ->
      case a1tyPrim of
        A1TyInt -> (Set.empty, Set.empty)
        A1TyFloat -> (Set.empty, Set.empty)
        A1TyBool -> (Set.empty, Set.empty)
        A1TyUnit -> (Set.empty, Set.empty)
        A1TyTensor a0eList -> frees a0eList
    A1TyList a1tye1 ->
      frees a1tye1
    A1TyArrow a1tye1 a1tye2 ->
      unionPairs [frees a1tye1, frees a1tye2]

  subst s = \case
    A1TyPrim a1tyPrim ->
      A1TyPrim $ case a1tyPrim of
        A1TyInt -> A1TyInt
        A1TyFloat -> A1TyFloat
        A1TyBool -> A1TyBool
        A1TyUnit -> A1TyUnit
        A1TyTensor a0eList -> A1TyTensor (go a0eList)
    A1TyList a1tye1 ->
      A1TyList (go a1tye1)
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
          (A1TyTensor a0eList1, A1TyTensor a0eList2) ->
            alphaEquivalent a0eList1 a0eList2
          (_, _) ->
            False
      (A1TyList a1tye1', A1TyList a1tye2') ->
        alphaEquivalent a1tye1' a1tye2'
      (A1TyArrow a1tye11 a1tye12, A1TyArrow a1tye21 a1tye22) ->
        alphaEquivalent a1tye11 a1tye21 && alphaEquivalent a1tye12 a1tye22
      (_, _) ->
        False

instance HasVar StrictAss0TypeExpr where
  frees = \case
    SA0TyPrim _ maybePred ->
      frees maybePred
    SA0TyList a0tye maybePred ->
      unionPairs [frees a0tye, frees maybePred]
    SA0TyArrow (yOpt, a0tye1) a0tye2 ->
      let (var0set1, var1set1) = frees a0tye1
          (var0set2, var1set2) = frees a0tye2
          var0set =
            Set.union var0set1 $
              case yOpt of
                Nothing -> var0set2
                Just y -> Set.delete y var0set2
          var1set = Set.union var1set1 var1set2
       in (var0set, var1set)
    SA0TyCode a1tye1 ->
      frees a1tye1

  subst s = \case
    SA0TyPrim a0tyPrim maybePred ->
      SA0TyPrim a0tyPrim (go maybePred)
    SA0TyList a0tye maybePred ->
      SA0TyList (go a0tye) (go maybePred)
    SA0TyArrow (yOpt, sa0tye1) sa0tye2 ->
      SA0TyArrow (yOpt, go sa0tye1) $
        case (yOpt, s) of
          (Just y, Subst0 x _) -> if y == x then sa0tye2 else go sa0tye2
          (Nothing, _) -> go sa0tye2
          (_, Subst1 _ _) -> go sa0tye2
    SA0TyCode a1tye1 ->
      SA0TyCode (go a1tye1)
    where
      go :: forall a. (HasVar a) => a -> a
      go = subst s

  alphaEquivalent sa0tye1 sa0tye2 =
    case (sa0tye1, sa0tye2) of
      (SA0TyPrim a0tyPrim1 maybePred1, SA0TyPrim a0tyPrim2 maybePred2) ->
        -- Exact match
        a0tyPrim1 == a0tyPrim2 && alphaEquivalent maybePred1 maybePred2
      (SA0TyArrow (y1opt, sa0tye11) sa0tye12, SA0TyArrow (y2opt, sa0tye21) sa0tye22) ->
        (alphaEquivalent sa0tye11 sa0tye21 &&) $
          case (y1opt, y2opt) of
            (Nothing, Nothing) ->
              alphaEquivalent sa0tye12 sa0tye22
            (Just y1, Nothing) ->
              not (occurs0 y1 sa0tye12) && alphaEquivalent sa0tye12 sa0tye22
            (Nothing, Just y2) ->
              not (occurs0 y2 sa0tye22) && alphaEquivalent sa0tye12 sa0tye22
            (Just y1, Just y2) ->
              alphaEquivalent sa0tye12 (subst0 (A0Var y1) y2 sa0tye22)
      (SA0TyCode a1tye1, SA0TyCode a1tye2) ->
        alphaEquivalent a1tye1 a1tye2
      (_, _) ->
        False

instance HasVar Type1Equation where
  frees = \case
    TyEq1Prim ty1eqPrim ->
      case ty1eqPrim of
        TyEq1Int -> (Set.empty, Set.empty)
        TyEq1Float -> (Set.empty, Set.empty)
        TyEq1Bool -> (Set.empty, Set.empty)
        TyEq1Unit -> (Set.empty, Set.empty)
        TyEq1TensorByLiteral zipped -> unionPairs (concatMap (\(a0e1, a0e2) -> [frees a0e1, frees a0e2]) zipped)
        TyEq1TensorByWhole a0eList1 a0eList2 -> unionPairs [frees a0eList1, frees a0eList2]
    TyEq1List ty1eqElem ->
      frees ty1eqElem
    TyEq1Arrow ty1eqDom ty1eqCod ->
      unionPairs [frees ty1eqDom, frees ty1eqCod]

  subst s = \case
    TyEq1Prim ty1eqPrim ->
      TyEq1Prim $
        case ty1eqPrim of
          TyEq1Int -> TyEq1Int
          TyEq1Float -> TyEq1Float
          TyEq1Bool -> TyEq1Bool
          TyEq1Unit -> TyEq1Unit
          TyEq1TensorByLiteral zipped -> TyEq1TensorByLiteral (map (both go) zipped)
          TyEq1TensorByWhole a0eList1 a0eList2 -> TyEq1TensorByWhole (go a0eList1) (go a0eList2)
    TyEq1List ty1eqElem ->
      TyEq1List (go ty1eqElem)
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
          (TyEq1TensorByLiteral zipped1, TyEq1TensorByLiteral zipped2) ->
            case zipExactMay zipped1 zipped2 of
              Nothing ->
                False
              Just zippedZipped ->
                all
                  ( \((a0e11, a0e12), (a0e21, a0e22)) ->
                      alphaEquivalent a0e11 a0e21 && alphaEquivalent a0e12 a0e22
                  )
                  zippedZipped
          (TyEq1TensorByWhole a0eList11 a0eList12, TyEq1TensorByWhole a0eList21 a0eList22) ->
            alphaEquivalent a0eList11 a0eList21 && alphaEquivalent a0eList12 a0eList22
          (_, _) ->
            False
      (TyEq1List ty1eqElem1, TyEq1List ty1eqElem2) ->
        alphaEquivalent ty1eqElem1 ty1eqElem2
      (TyEq1Arrow ty1eqDom1 ty1eqCod1, TyEq1Arrow ty1eqDom2 ty1eqCod2) ->
        alphaEquivalent ty1eqDom1 ty1eqDom2
          && alphaEquivalent ty1eqCod1 ty1eqCod2
      (_, _) ->
        False

instance (HasVar a) => HasVar (Maybe a) where
  frees Nothing = (Set.empty, Set.empty)
  frees (Just v) = frees v

  subst s = fmap (subst s)

  alphaEquivalent Nothing Nothing = True
  alphaEquivalent (Just v1) (Just v2) = alphaEquivalent v1 v2
  alphaEquivalent _ _ = False

instance (HasVar a) => HasVar (Result a) where
  frees = \case
    Pure v -> frees v
    Cast0 cast a0tye r -> unionPairs [frees cast, frees a0tye, frees r]
    Cast1 eq a1tye r -> unionPairs [frees eq, frees a1tye, frees r]
    CastGiven0 cast a0tye r -> unionPairs [frees cast, frees a0tye, frees r]
    FillInferred0 a0e r -> unionPairs [frees a0e, frees r]
    InsertInferred0 a0e r -> unionPairs [frees a0e, frees r]

  subst s = \case
    Pure v -> Pure (go v)
    Cast0 cast a0tye r -> Cast0 (go cast) (go a0tye) (go r)
    Cast1 eq a1tye r -> Cast1 (go eq) (go a1tye) (go r)
    CastGiven0 cast a0tye r -> CastGiven0 (go cast) (go a0tye) (go r)
    FillInferred0 a0e r -> FillInferred0 (go a0e) (go r)
    InsertInferred0 a0e r -> InsertInferred0 (go a0e) (go r)
    where
      go :: forall b. (HasVar b) => b -> b
      go = subst s

  alphaEquivalent =
    error "TODO (enhance): Result a, alphaEquivalent"
