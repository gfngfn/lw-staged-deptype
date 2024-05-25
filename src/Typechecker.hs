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
import Prelude hiding (mod)

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
    let a0tye1' = substTypeExpr0 a0e x a0tye1
     in A0Lam (y, a0tye1') (if y == x then a0e2 else go0 a0e2)
  A0App a0e1 a0e2 ->
    A0App (go0 a0e1) (go0 a0e2)
  A0Bracket a1e1 ->
    A0Bracket (go1 a1e1)
  A0AssertAndThen a0e1 a0e2 a0e0 ->
    A0AssertAndThen (go0 a0e1) (go0 a0e2) (go0 a0e0)
  where
    go0 = substExpr0 a0e x
    go1 = substExpr1 a0e x

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

extractEquations0 :: trav -> Ass0TypeExpr -> Ass0TypeExpr -> M trav [(Ass0Expr, Ass0Expr)]
extractEquations0 trav a0tye1 a0tye2 =
  case (a0tye1, a0tye2) of
    (A0TyPrim a0tyPrim1, A0TyPrim a0tyPrim2) ->
      case (a0tyPrim1, a0tyPrim2) of
        (A0TyInt, A0TyInt) -> pure []
        (A0TyBool, A0TyBool) -> pure []
        _ -> typeError trav $ TypeContradictionAtStage0 a0tye1 a0tye2
    (A0TyArrow (_x1opt, a0tye11) _a0tye12, A0TyArrow (_x2opt, a0tye21) _a0tye22) -> do
      _eqns1 <- extractEquations0 trav a0tye11 a0tye21
      error "TODO: extractEquations0, A0TyArrow"
    (A0TyCode a1tye1, A0TyCode a1tye2) ->
      extractEquations1 trav a1tye1 a1tye2
    _ ->
      typeError trav $ TypeContradictionAtStage0 a0tye1 a0tye2

extractEquations1 :: trav -> Ass1TypeExpr -> Ass1TypeExpr -> M trav [(Ass0Expr, Ass0Expr)]
extractEquations1 trav a1tye1 a1tye2 =
  case (a1tye1, a1tye2) of
    (A1TyPrim a1tyPrim1, A1TyPrim a1tyPrim2) ->
      case (a1tyPrim1, a1tyPrim2) of
        (A1TyInt, A1TyInt) -> pure []
        (A1TyBool, A1TyBool) -> pure []
        (A1TyVec a0e1, A1TyVec a0e2) -> pure [(a0e1, a0e2)]
        _ -> typeError trav $ TypeContradictionAtStage1 a1tye1 a1tye2
    (A1TyArrow a1tye11 a1tye12, A1TyArrow a1tye21 a1tye22) -> do
      eqns1 <- extractEquations1 trav a1tye11 a1tye21
      eqns2 <- extractEquations1 trav a1tye12 a1tye22
      pure $ eqns1 ++ eqns2
    _ ->
      typeError trav $ TypeContradictionAtStage1 a1tye1 a1tye2

addAssertions0 :: trav -> Ass0TypeExpr -> Ass0TypeExpr -> Ass0Expr -> M trav Ass0Expr
addAssertions0 trav a0tye1 a0tye2 a0e0 = do
  eqns <- extractEquations0 trav a0tye1 a0tye2
  pure $
    foldr
      (\(lhs, rhs) a0e -> A0AssertAndThen lhs rhs a0e)
      a0e0
      eqns

addAssertions1 :: trav -> Ass1TypeExpr -> Ass1TypeExpr -> Ass1Expr -> M trav Ass1Expr
addAssertions1 trav a1tye1 a1tye2 a1e0 = do
  eqns <- extractEquations1 trav a1tye1 a1tye2
  pure . A1Escape $
    foldr
      (\(lhs, rhs) a0e -> A0AssertAndThen lhs rhs a0e)
      (A0Bracket a1e0)
      eqns

typecheckExpr0 :: trav -> TypeEnv -> Expr -> M trav (Ass0TypeExpr, Ass0Expr)
typecheckExpr0 trav tyEnv = \case
  Literal lit ->
    case lit of
      LitInt _ -> pure (A0TyPrim A0TyInt, A0Literal lit)
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
        a0e2' <- addAssertions0 trav a0tye11 a0tye2 a0e2
        let a0tye12' =
              case x11opt of
                Just x11 -> substTypeExpr0 a0e2 x11 a0tye12
                Nothing -> a0tye12
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
  Literal lit ->
    case lit of
      LitInt _ -> pure (A1TyPrim A1TyInt, A1Literal lit)
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
        a1e2' <- addAssertions1 trav a1tye11 a1tye2 a1e2
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
        _ -> typeError trav $ UnknownTypeOrInvalidArity tyName (List.length results)
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
        ("Vec", [(A0TyPrim A0TyInt, a0e)]) -> pure $ A1TyVec a0e
        _ -> typeError trav $ UnknownTypeOrInvalidArity tyName (List.length results)
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
