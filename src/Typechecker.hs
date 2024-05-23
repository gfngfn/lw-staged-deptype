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

findVar :: trav -> Var -> TypeEnv -> M trav TypeExpr
findVar trav x tyEnv =
  lift $ maybeToEither (UnboundVar x, trav) $ TypeEnv.findVar x tyEnv

typecheckExpr0 :: trav -> TypeEnv -> Expr -> M trav (TypeExpr, AssExpr0)
typecheckExpr0 trav tyEnv = \case
  Var x -> do
    tye <- findVar trav x tyEnv
    pure (tye, Var x)
  Lam (x1, tye1) e2 -> do
    atye1 <- typecheckTypeExpr0 trav tyEnv tye1
    (atye2, ae2) <- typecheckExpr0 trav (TypeEnv.addVar x1 atye1 tyEnv) e2
    pure (TyArrow (Just x1, atye1) atye2, Lam (x1, atye1) ae2)
  App e1 e2 -> do
    (atye1, ae1) <- typecheckExpr0 trav tyEnv e1
    (atye2, ae2) <- typecheckExpr0 trav tyEnv e2
    case atye1 of
      TyArrow (_x11, atye11) atye12 ->
        if atye11 == atye2
          then pure (atye12, App ae1 ae2) -- TODO: do substitution and insert assertion here
          else typeError trav $ TypeContradiction atye11 atye2
      _ ->
        typeError trav $ NotAFunctionTypeForStage0 atye1
  Bracket e1 -> do
    (atye1, ae1) <- typecheckExpr1 trav tyEnv e1
    pure (TyCode atye1, Bracket ae1)
  Escape _ ->
    typeError trav CannotUseEscapeAtStage0

typecheckExpr1 :: trav -> TypeEnv -> Expr -> M trav (TypeExpr, AssExpr1)
typecheckExpr1 trav tyEnv = \case
  Var x -> do
    tye <- findVar trav x tyEnv
    pure (tye, Var x)
  Lam (x1, tye1) e2 -> do
    atye1 <- typecheckTypeExpr1 trav tyEnv tye1
    (atye2, ae2) <- typecheckExpr1 trav (TypeEnv.addVar x1 atye1 tyEnv) e2
    pure (TyArrow (Just x1, atye1) atye2, Lam (x1, atye1) ae2)
  App e1 e2 -> do
    (atye1, ae1) <- typecheckExpr1 trav tyEnv e1
    (atye2, ae2) <- typecheckExpr1 trav tyEnv e2
    case atye1 of
      TyArrow (Nothing, atye11) atye12 ->
        if atye11 == atye2
          then pure (atye12, App ae1 ae2)
          else typeError trav $ TypeContradiction atye11 atye2
      _ ->
        typeError trav $ NotAFunctionTypeForStage1 atye1
  Bracket _ ->
    typeError trav CannotUseBracketAtStage1
  Escape e1 -> do
    (atye1, ae1) <- typecheckExpr0 trav tyEnv e1
    case atye1 of
      TyCode atye -> pure (atye, Escape ae1)
      _ -> typeError trav $ NotACodeType atye1

typecheckTypeExpr0 :: trav -> TypeEnv -> TypeExpr -> M trav AssTypeExpr0
typecheckTypeExpr0 trav tyEnv = \case
  TyName tyName es -> do
    results <- mapM (typecheckExpr0 trav tyEnv) es
    --     baseTy <-
    --       case (tyName, results) of
    --         ("int", []) -> pure TyBaseInt
    --         ("bool", []) -> pure TyBaseBool
    --         _ -> typeError trav $ UnknownTypeOrInvalidArity tyName (List.length semTys)
    -- TODO
    let aes = snd <$> results
    pure $ TyName tyName aes
  TyArrow (xOpt, tye1) tye2 -> do
    atye1 <- typecheckTypeExpr0 trav tyEnv tye1
    atye2 <-
      case xOpt of
        Just x -> typecheckTypeExpr0 trav (TypeEnv.addVar x atye1 tyEnv) tye2
        Nothing -> typecheckTypeExpr0 trav tyEnv tye2
    pure $ TyArrow (xOpt, atye1) atye2
  TyCode tye1 -> do
    atye1 <- typecheckTypeExpr1 trav tyEnv tye1
    pure $ TyCode atye1

typecheckTypeExpr1 :: trav -> TypeEnv -> TypeExpr -> M trav AssTypeExpr1
typecheckTypeExpr1 trav tyEnv = \case
  TyName tyName es -> do
    results <- mapM (typecheckExpr0 trav tyEnv) es
    --     baseTy <-
    --       case (tyName, results) of
    --         ("int", []) -> pure TyBaseInt
    --         ("bool", []) -> pure TyBaseBool
    --         _ -> typeError trav $ UnknownTypeOrInvalidArity tyName (List.length semTys)
    -- TODO
    let aes = snd <$> results
    pure $ TyName tyName aes
  TyArrow (xOpt, tye1) tye2 -> do
    atye1 <- typecheckTypeExpr0 trav tyEnv tye1
    () <-
      case xOpt of
        Just x -> typeError trav $ FunctionTypeCannotBeDependentAtStage1 x
        Nothing -> pure ()
    atye2 <- typecheckTypeExpr0 trav tyEnv tye2
    pure $ TyArrow (Nothing, atye1) atye2
  TyCode _ -> do
    typeError trav CannotUseCodeTypeAtStage1
