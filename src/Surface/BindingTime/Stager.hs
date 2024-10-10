module Surface.BindingTime.Stager
  ( BCExpr,
    BCExprMain,
    BCTypeExpr,
    BCTypeExprMain,
    stageExpr0,
  )
where

import Lwsd.Syntax qualified as Lwsd
import Surface.BindingTime.Core
import Surface.Syntax
import Util.TokenUtil
import Prelude

type BCExpr = ExprF (BindingTimeConst, Span)

type BCExprMain = ExprMainF (BindingTimeConst, Span)

type BCTypeExpr = TypeExprF (BindingTimeConst, Span)

type BCTypeExprMain = TypeExprMainF (BindingTimeConst, Span)

stageExpr0 :: BCExpr -> Lwsd.Expr
stageExpr0 (Expr (btc, ann) exprMain) =
  case btc of
    BT0 -> Lwsd.Expr ann (stageExpr0Main exprMain)
    BT1 -> Lwsd.Expr ann (Lwsd.Bracket (Lwsd.Expr ann (stageExpr1Main exprMain)))

stageExpr0Main :: BCExprMain -> Lwsd.ExprMain
stageExpr0Main = \case
  Literal lit -> Lwsd.Literal (convertLiteral lit)
  Var x -> Lwsd.Var x
  Lam (x, tye1) e2 -> Lwsd.Lam (x, stageTypeExpr0 tye1) (stageExpr0 e2)
  App e1 e2 -> Lwsd.App (stageExpr0 e1) (stageExpr0 e2)
  LetIn x e1 e2 -> Lwsd.LetIn x (stageExpr0 e1) (stageExpr0 e2)

stageExpr1 :: BCExpr -> Lwsd.Expr
stageExpr1 (Expr (btc, ann) exprMain) =
  case btc of
    BT0 -> Lwsd.Expr ann (Lwsd.Escape (Lwsd.Expr ann (stageExpr0Main exprMain)))
    BT1 -> Lwsd.Expr ann (stageExpr1Main exprMain)

stageExpr1Main :: BCExprMain -> Lwsd.ExprMain
stageExpr1Main = \case
  Literal lit -> Lwsd.Literal (convertLiteral lit)
  Var x -> Lwsd.Var x
  Lam (x, tye1) e2 -> Lwsd.Lam (x, stageTypeExpr1 tye1) (stageExpr1 e2)
  App e1 e2 -> Lwsd.App (stageExpr1 e1) (stageExpr1 e2)
  LetIn x e1 e2 -> Lwsd.LetIn x (stageExpr1 e1) (stageExpr1 e2)

stageTypeExpr0 :: BCTypeExpr -> Lwsd.TypeExpr
stageTypeExpr0 (TypeExpr (btc, ann) typeExprMain) =
  case btc of
    BT1 -> Lwsd.TypeExpr ann (Lwsd.TyCode (Lwsd.TypeExpr ann (stageTypeExpr1Main typeExprMain)))
    BT0 -> Lwsd.TypeExpr ann (stageTypeExpr0Main typeExprMain)

stageTypeExpr0Main :: BCTypeExprMain -> Lwsd.TypeExprMain
stageTypeExpr0Main = \case
  TyName tyName args ->
    case args of
      [] -> Lwsd.TyName tyName []
      _ : _ -> error "stageTypeExpr0Main, non-empty `args`"
  TyArrow (xOpt, tye1) tye2 ->
    Lwsd.TyArrow (xOpt, stageTypeExpr0 tye1) (stageTypeExpr0 tye2)

stageTypeExpr1 :: BCTypeExpr -> Lwsd.TypeExpr
stageTypeExpr1 (TypeExpr (btc, ann) typeExprMain) =
  case btc of
    BT0 -> error "stageTypeExpr1, BT0"
    BT1 -> Lwsd.TypeExpr ann (stageTypeExpr1Main typeExprMain)

stageTypeExpr1Main :: BCTypeExprMain -> Lwsd.TypeExprMain
stageTypeExpr1Main = \case
  TyName tyName args -> Lwsd.TyName tyName (map (Lwsd.PersistentArg . stageExpr0) args)
  TyArrow (xOpt, tye1) tye2 -> Lwsd.TyArrow (xOpt, stageTypeExpr1 tye1) (stageTypeExpr1 tye2)

convertLiteral :: Literal -> Lwsd.Literal
convertLiteral = \case
  LitInt n -> Lwsd.LitInt n
  LitVec ns -> Lwsd.LitVec ns
  LitMat nss -> Lwsd.LitMat nss
