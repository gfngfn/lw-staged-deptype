module Typechecker
  ( typecheckExpr,
    typecheckTypeExpr,
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

typecheckExpr :: trav -> TypeEnv -> Expr -> M trav (TypeExpr, AssExpr)
typecheckExpr trav tyEnv = \case
  Var x -> do
    tye <- findVar trav x tyEnv
    pure (tye, Var x)
  Lam (x1, tye1) e2 -> do
    atye1 <- typecheckTypeExpr trav tyEnv tye1
    (atye2, ae2) <- typecheckExpr trav (TypeEnv.addVar x1 atye1 tyEnv) e2
    pure (TyArrow (x1, atye1) atye2, Lam (x1, atye1) ae2)
  App e1 e2 -> do
    (atye1, ae1) <- typecheckExpr trav tyEnv e1
    (atye2, ae2) <- typecheckExpr trav tyEnv e2
    case atye1 of
      TyArrow (_x11, atye11) atye12 ->
        if atye11 == atye2
          then pure (atye12, App ae1 ae2)
          else typeError trav $ TypeContradiction atye11 atye2
      _ ->
        typeError trav $ NotAFunctionType atye1

typecheckTypeExpr :: trav -> TypeEnv -> TypeExpr -> M trav AssTypeExpr
typecheckTypeExpr trav tyEnv = \case
  TyName tyName es -> do
    results <- mapM (typecheckExpr trav tyEnv) es
    --     baseTy <-
    --       case (tyName, results) of
    --         ("int", []) -> pure TyBaseInt
    --         ("bool", []) -> pure TyBaseBool
    --         _ -> typeError trav $ UnknownTypeOrInvalidArity tyName (List.length semTys)
    -- TODO
    let aes = snd <$> results
    pure $ TyName tyName aes
  TyArrow (x, tye1) tye2 -> do
    atye1 <- typecheckTypeExpr trav tyEnv tye1
    atye2 <- typecheckTypeExpr trav (TypeEnv.addVar x atye1 tyEnv) tye2
    pure $ TyArrow (x, atye1) atye2
