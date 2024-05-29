module Typechecker
  ( typecheckExpr0,
    typecheckExpr1,
    typecheckTypeExpr0,
    typecheckTypeExpr1,
    TypecheckerState,
    M,
  )
where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Either.Extra
import Data.List qualified as List
import Syntax
import TypeEnv (TypeEnv)
import TypeEnv qualified
import TypeError
import Vector qualified
import Prelude hiding (mod)

-- TODO: make this changeable by command lines
optimizeTrivialAssertion :: Bool
optimizeTrivialAssertion = True

type TypecheckerState = ()

type M trav a = StateT TypecheckerState (Either (TypeError, trav)) a

typeError :: trav -> TypeError -> M trav b
typeError trav e = lift $ Left (e, trav)

findVar :: trav -> Var -> TypeEnv -> M trav TypeEnv.Entry
findVar trav x tyEnv =
  lift $ maybeToEither (UnboundVar x, trav) $ TypeEnv.findVar x tyEnv

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
  A0TyEqAssert ty0eq a0e0 ->
    A0TyEqAssert (goTyEq0 ty0eq) (go0 a0e0)
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

makeEquation0 :: trav -> Ass0TypeExpr -> Ass0TypeExpr -> M trav Type0Equality
makeEquation0 trav a0tye1 a0tye2 =
  case (a0tye1, a0tye2) of
    (A0TyPrim a0tyPrim1, A0TyPrim a0tyPrim2) ->
      case (a0tyPrim1, a0tyPrim2) of
        (A0TyInt, A0TyInt) -> pure TyEq0PrimInt
        (A0TyBool, A0TyBool) -> pure TyEq0PrimBool
        (A0TyVec n1, A0TyVec n2) | n1 == n2 -> pure $ TyEq0PrimVec n1
        _ -> typeError trav $ TypeContradictionAtStage0 a0tye1 a0tye2
    (A0TyArrow (x1opt, a0tye11) a0tye12, A0TyArrow (x2opt, a0tye21) a0tye22) -> do
      case (x1opt, x2opt) of
        (Nothing, Nothing) -> do
          ty0eqDom <- makeEquation0 trav a0tye11 a0tye21
          ty0eqCod <- makeEquation0 trav a0tye12 a0tye22
          pure $ TyEq0Arrow Nothing ty0eqDom ty0eqCod
        (Just x1, Nothing) -> do
          ty0eqDom <- makeEquation0 trav a0tye11 a0tye21
          ty0eqCod <- makeEquation0 trav a0tye12 a0tye22
          pure $ TyEq0Arrow (Just x1) ty0eqDom ty0eqCod
        (Nothing, Just x2) -> do
          ty0eqDom <- makeEquation0 trav a0tye11 a0tye21
          ty0eqCod <- makeEquation0 trav a0tye12 a0tye22
          pure $ TyEq0Arrow (Just x2) ty0eqDom ty0eqCod
        (Just x1, Just x2) -> do
          ty0eqDom <- makeEquation0 trav a0tye11 a0tye21
          ty0eqCod <- makeEquation0 trav a0tye12 (substTypeExpr0 (A0Var x1) x2 a0tye22)
          pure $ TyEq0Arrow (Just x1) ty0eqDom ty0eqCod
    (A0TyCode a1tye1, A0TyCode a1tye2) -> do
      ty1eq <- makeEquation1 trav a1tye1 a1tye2
      pure $ TyEq0Code ty1eq
    _ ->
      typeError trav $ TypeContradictionAtStage0 a0tye1 a0tye2

makeEquation1 :: trav -> Ass1TypeExpr -> Ass1TypeExpr -> M trav Type1Equality
makeEquation1 trav a1tye1 a1tye2 =
  case (a1tye1, a1tye2) of
    (A1TyPrim a1tyPrim1, A1TyPrim a1tyPrim2) ->
      case (a1tyPrim1, a1tyPrim2) of
        (A1TyInt, A1TyInt) -> pure TyEq1PrimInt
        (A1TyBool, A1TyBool) -> pure TyEq1PrimBool
        (A1TyVec a0e1, A1TyVec a0e2) -> pure $ TyEq1PrimVec a0e1 a0e2
        _ -> typeError trav $ TypeContradictionAtStage1 a1tye1 a1tye2
    (A1TyArrow a1tye11 a1tye12, A1TyArrow a1tye21 a1tye22) -> do
      ty1eqDom <- makeEquation1 trav a1tye11 a1tye21
      ty1eqCod <- makeEquation1 trav a1tye12 a1tye22
      pure $ TyEq1Arrow ty1eqDom ty1eqCod
    _ ->
      typeError trav $ TypeContradictionAtStage1 a1tye1 a1tye2

typecheckExpr0 :: trav -> TypeEnv -> Expr -> M trav (Ass0TypeExpr, Ass0Expr)
typecheckExpr0 trav tyEnv = \case
  Literal lit -> do
    let a0tye =
          case lit of
            LitInt _ -> A0TyPrim A0TyInt
            LitVec v -> A0TyPrim (A0TyVec (Vector.length v))
    pure (a0tye, A0Literal lit)
  Var x -> do
    entry <- findVar trav x tyEnv
    case entry of
      TypeEnv.Ass0Entry a0tye -> pure (a0tye, A0Var x)
      TypeEnv.Ass1Entry _ -> typeError trav $ NotAStage0Var x
  Lam (x1, tye1) e2 -> do
    a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
    (a0tye2, a0e2) <- typecheckExpr0 trav (TypeEnv.addVar x1 (TypeEnv.Ass0Entry a0tye1) tyEnv) e2
    pure (A0TyArrow (Just x1, a0tye1) a0tye2, A0Lam (x1, a0tye1) a0e2)
  App e1 e2 -> do
    (a0tye1, a0e1) <- typecheckExpr0 trav tyEnv e1
    (a0tye2, a0e2) <- typecheckExpr0 trav tyEnv e2
    case a0tye1 of
      A0TyArrow (x11opt, a0tye11) a0tye12 -> do
        ty0eq <- makeEquation0 trav a0tye11 a0tye2
        let a0tye12' =
              case x11opt of
                Just x11 -> substTypeExpr0 a0e2 x11 a0tye12
                Nothing -> a0tye12
        let a0e2' =
              if optimizeTrivialAssertion && a0tye11 == a0tye2
                then a0e2 -- Do slight shortcuts
                else A0TyEqAssert ty0eq a0e2
        pure (a0tye12', A0App a0e1 a0e2')
      _ ->
        typeError trav $ NotAFunctionTypeForStage0 a0tye1
  LetIn x e1 e2 -> do
    (a0tye1, a0e1) <- typecheckExpr0 trav tyEnv e1
    (a0tye2, a0e2) <- typecheckExpr0 trav (TypeEnv.addVar x (TypeEnv.Ass0Entry a0tye1) tyEnv) e2
    pure (a0tye2, A0App (A0Lam (x, a0tye1) a0e2) a0e1)
  -- TODO: check that `x` does not occur in `a0tye2`
  Bracket e1 -> do
    (a1tye1, a1e1) <- typecheckExpr1 trav tyEnv e1
    pure (A0TyCode a1tye1, A0Bracket a1e1)
  Escape _ ->
    typeError trav CannotUseEscapeAtStage0

typecheckExpr1 :: trav -> TypeEnv -> Expr -> M trav (Ass1TypeExpr, Ass1Expr)
typecheckExpr1 trav tyEnv = \case
  Literal lit -> do
    let a1tye =
          case lit of
            LitInt _ -> A1TyPrim A1TyInt
            LitVec v -> A1TyPrim (A1TyVec (A0Literal (LitInt (Vector.length v))))
    pure (a1tye, A1Literal lit)
  Var x -> do
    entry <- findVar trav x tyEnv
    case entry of
      TypeEnv.Ass0Entry _ -> typeError trav $ NotAStage1Var x
      TypeEnv.Ass1Entry a1tye -> pure (a1tye, A1Var x)
  Lam (x1, tye1) e2 -> do
    a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
    (a1tye2, a1e2) <- typecheckExpr1 trav (TypeEnv.addVar x1 (TypeEnv.Ass1Entry a1tye1) tyEnv) e2
    pure (A1TyArrow a1tye1 a1tye2, A1Lam (x1, a1tye1) a1e2)
  App e1 e2 -> do
    (a1tye1, a1e1) <- typecheckExpr1 trav tyEnv e1
    (a1tye2, a1e2) <- typecheckExpr1 trav tyEnv e2
    case a1tye1 of
      A1TyArrow a1tye11 a1tye12 -> do
        -- Embeds type equality assertion at stage 0 here!
        ty1eq <- makeEquation1 trav a1tye11 a1tye2
        let ty0eq = TyEq0Code ty1eq
        let a1e2' =
              if optimizeTrivialAssertion && a1tye11 == a1tye2
                then a1e2 -- Do slight shortcuts
                else (A1Escape (A0TyEqAssert ty0eq (A0Bracket a1e2)))
        pure (a1tye12, A1App a1e1 a1e2')
      _ ->
        typeError trav $ NotAFunctionTypeForStage1 a1tye1
  LetIn x e1 e2 -> do
    (a1tye1, a1e1) <- typecheckExpr1 trav tyEnv e1
    (a1tye2, a1e2) <- typecheckExpr1 trav (TypeEnv.addVar x (TypeEnv.Ass1Entry a1tye1) tyEnv) e2
    pure (a1tye2, A1App (A1Lam (x, a1tye1) a1e2) a1e1)
  -- TODO: check that `x` does not occur in `a0tye2`
  Bracket _ ->
    typeError trav CannotUseBracketAtStage1
  Escape e1 -> do
    (a0tye1, a0e1) <- typecheckExpr0 trav tyEnv e1
    case a0tye1 of
      A0TyCode a1tye -> pure (a1tye, A1Escape a0e1)
      _ -> typeError trav $ NotACodeType a0tye1

typecheckTypeExpr0 :: trav -> TypeEnv -> TypeExpr -> M trav Ass0TypeExpr
typecheckTypeExpr0 trav tyEnv = \case
  TyName tyName args -> do
    results <-
      mapM
        ( \case
            PersistentArg _ -> typeError trav CannotUsePersistentArgAtStage0
            NormalArg e -> typecheckExpr0 trav tyEnv e
        )
        args
    tyPrim <-
      case (tyName, results) of
        ("Int", []) -> pure A0TyInt
        ("Bool", []) -> pure A0TyBool
        ("Vec", [arg]) ->
          case arg of
            (A0TyPrim A0TyInt, A0Literal (LitInt n)) -> pure $ A0TyVec n
            (_a0tye, a0e) -> typeError trav $ NotAnIntLitArgOfVecAtStage0 a0e
        _ -> typeError trav $ UnknownTypeOrInvalidArityAtStage0 tyName (List.length results)
    pure $ A0TyPrim tyPrim
  TyArrow (xOpt, tye1) tye2 -> do
    a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
    a0tye2 <-
      case xOpt of
        Just x -> typecheckTypeExpr0 trav (TypeEnv.addVar x (TypeEnv.Ass0Entry a0tye1) tyEnv) tye2
        Nothing -> typecheckTypeExpr0 trav tyEnv tye2
    pure $ A0TyArrow (xOpt, a0tye1) a0tye2
  TyCode tye1 -> do
    a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
    pure $ A0TyCode a1tye1

typecheckTypeExpr1 :: trav -> TypeEnv -> TypeExpr -> M trav Ass1TypeExpr
typecheckTypeExpr1 trav tyEnv = \case
  TyName tyName args -> do
    results <-
      mapM
        ( \case
            PersistentArg e -> typecheckExpr0 trav tyEnv e
            NormalArg _ -> typeError trav CannotUseNormalArgAtStage1
        )
        args
    a1tyPrim <-
      case (tyName, results) of
        ("Int", []) -> pure A1TyInt
        ("Bool", []) -> pure A1TyBool
        ("Vec", [(a0tye, a0e)]) ->
          case a0tye of
            A0TyPrim A0TyInt -> pure $ A1TyVec a0e
            _ -> typeError trav $ NotAnIntTypedArgOfVecAtStage1 a0tye
        _ -> typeError trav $ UnknownTypeOrInvalidArityAtStage1 tyName (List.length results)
    pure $ A1TyPrim a1tyPrim
  TyArrow (xOpt, tye1) tye2 -> do
    a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
    () <-
      case xOpt of
        Just x -> typeError trav $ FunctionTypeCannotBeDependentAtStage1 x
        Nothing -> pure ()
    a1tye2 <- typecheckTypeExpr1 trav tyEnv tye2
    pure $ A1TyArrow a1tye1 a1tye2
  TyCode _ -> do
    typeError trav CannotUseCodeTypeAtStage1
