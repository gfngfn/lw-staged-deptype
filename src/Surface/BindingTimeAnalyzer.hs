module Surface.BindingTimeAnalyzer
  ( BindingTimeVar,
    initialState,
    assignBindingTimeVarToExpr,
  )
where

import Control.Monad.Trans.State
import Surface.Syntax
import Util.TokenUtil (Span)
import Prelude hiding (succ)

newtype BindingTimeVar = BindingTimeVar Int
  deriving stock (Show)

initialState :: BindingTimeVar
initialState = BindingTimeVar 0

succ :: BindingTimeVar -> BindingTimeVar
succ (BindingTimeVar n) = BindingTimeVar (n + 1)

type Assigner a = State BindingTimeVar a

fresh :: Assigner BindingTimeVar
fresh = do
  btv <- get
  put (succ btv)
  pure btv

type BExpr = ExprF (BindingTimeVar, Span)

type BTypeExpr = TypeExprF (BindingTimeVar, Span)

assignBindingTimeVarToExpr :: Expr -> Assigner BExpr
assignBindingTimeVarToExpr (Expr ann exprMain) = do
  btv <- fresh
  Expr (btv, ann)
    <$> case exprMain of
      Literal lit -> pure $ Literal lit
      Var x -> pure $ Var x
      Lam (x, ty) e -> do
        bty <- assignBindingTimeVarToTypeExpr ty
        be <- assignBindingTimeVarToExpr e
        pure $ Lam (x, bty) be
      App e1 e2 -> do
        be1 <- assignBindingTimeVarToExpr e1
        be2 <- assignBindingTimeVarToExpr e2
        pure $ App be1 be2
      LetIn x e1 e2 -> do
        be1 <- assignBindingTimeVarToExpr e1
        be2 <- assignBindingTimeVarToExpr e2
        pure $ LetIn x be1 be2

assignBindingTimeVarToTypeExpr :: TypeExpr -> Assigner BTypeExpr
assignBindingTimeVarToTypeExpr (TypeExpr ann typeExprMain) = do
  btv <- fresh
  TypeExpr (btv, ann)
    <$> case typeExprMain of
      TyName tyName es -> do
        bes <- mapM assignBindingTimeVarToExpr es
        pure $ TyName tyName bes
      TyArrow (xOpt, ty1) ty2 -> do
        bty1 <- assignBindingTimeVarToTypeExpr ty1
        bty2 <- assignBindingTimeVarToTypeExpr ty2
        pure $ TyArrow (xOpt, bty1) bty2
