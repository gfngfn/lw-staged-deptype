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
import Data.Tuple.Extra
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

typecheckExpr0 :: trav -> TypeEnv -> Expr -> M trav (Ass0TypeExpr, Ass0Expr)
typecheckExpr0 trav tyEnv = \case
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
      A0TyArrow (_x11, a0tye11) a0tye12 ->
        if a0tye11 == a0tye2
          then pure (a0tye12, A0App a0e1 a0e2) -- TODO: do substitution and insert assertion here
          else typeError trav $ TypeContradictionAtStage0 a0tye11 a0tye2
      _ ->
        typeError trav $ NotAFunctionTypeForStage0 a0tye1
  Bracket e1 -> do
    (a1tye1, a1e1) <- typecheckExpr1 trav tyEnv e1
    pure (A0TyCode a1tye1, A0Bracket a1e1)
  Escape _ ->
    typeError trav CannotUseEscapeAtStage0

typecheckExpr1 :: trav -> TypeEnv -> Expr -> M trav (Ass1TypeExpr, Ass1Expr)
typecheckExpr1 trav tyEnv = \case
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
      A1TyArrow a1tye11 a1tye12 ->
        if a1tye11 == a1tye2
          then pure (a1tye12, A1App a1e1 a1e2)
          else typeError trav $ TypeContradictionAtStage1 a1tye11 a1tye2
      _ ->
        typeError trav $ NotAFunctionTypeForStage1 a1tye1
  Bracket _ ->
    typeError trav CannotUseBracketAtStage1
  Escape e1 -> do
    (a0tye1, a0e1) <- typecheckExpr0 trav tyEnv e1
    case a0tye1 of
      A0TyCode a1tye -> pure (a1tye, A1Escape a0e1)
      _ -> typeError trav $ NotACodeType a0tye1

typecheckTypeExpr0 :: trav -> TypeEnv -> TypeExpr -> M trav Ass0TypeExpr
typecheckTypeExpr0 trav tyEnv = \case
  TyName tyName es -> do
    results <- mapM (typecheckExpr0 trav tyEnv) es
    --     baseTy <-
    --       case (tyName, results) of
    --         ("int", []) -> pure TyBaseInt
    --         ("bool", []) -> pure TyBaseBool
    --         _ -> typeError trav $ UnknownTypeOrInvalidArity tyName (List.length semTys)
    -- TODO
    let a0es = snd <$> results
    pure $ A0TyName tyName a0es
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
  TyName tyName es -> do
    results <- mapM (typecheckExpr0 trav tyEnv) es
    --     baseTy <-
    --       case (tyName, results) of
    --         ("int", []) -> pure TyBaseInt
    --         ("bool", []) -> pure TyBaseBool
    --         _ -> typeError trav $ UnknownTypeOrInvalidArity tyName (List.length semTys)
    -- TODO
    let a0es = snd <$> results
    pure $ A1TyName tyName a0es
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
