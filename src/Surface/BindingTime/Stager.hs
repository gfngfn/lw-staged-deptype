module Surface.BindingTime.Stager
  ( BCExprF,
    BCExprMainF,
    BCTypeExprF,
    BCTypeExprMainF,
    BCArgForTypeF,
    stageExpr0,
  )
where

import Lwsd.SrcSyntax qualified as Lwsd
import Surface.BindingTime.Core
import Surface.Syntax
import Prelude

type BCExprF ann = ExprF (BindingTimeConst, ann)

type BCExprMainF ann = ExprMainF (BindingTimeConst, ann)

type BCTypeExprF ann = TypeExprF (BindingTimeConst, ann)

type BCTypeExprMainF ann = TypeExprMainF (BindingTimeConst, ann)

type BCArgForTypeF ann = ArgForTypeF (BindingTimeConst, ann)

stageExpr0 :: BCExprF ann -> Lwsd.ExprF ann
stageExpr0 (Expr (btc, ann) exprMain) =
  case btc of
    BT0 -> Lwsd.Expr ann (stageExpr0Main exprMain)
    BT1 -> Lwsd.Expr ann (Lwsd.Bracket (Lwsd.Expr ann (stageExpr1Main exprMain)))

stageExpr0Main :: BCExprMainF ann -> Lwsd.ExprMainF ann
stageExpr0Main = \case
  Literal lit ->
    Lwsd.Literal (convertLiteral stageExpr0 lit)
  Var x ->
    Lwsd.Var ([], x) -- TODO: fix this about module name chains
  Lam Nothing (x, tye1) e2 ->
    Lwsd.Lam Nothing (x, stageTypeExpr0 tye1) (stageExpr0 e2)
  Lam (Just (f, tyeRec)) (x, tye1) e2 ->
    Lwsd.Lam (Just (f, stageTypeExpr0 tyeRec)) (x, stageTypeExpr0 tye1) (stageExpr0 e2)
  App e1 e2 ->
    Lwsd.App (stageExpr0 e1) (stageExpr0 e2)
  LetIn x e1 e2 ->
    Lwsd.LetIn x (stageExpr0 e1) (stageExpr0 e2)
  IfThenElse e0 e1 e2 ->
    Lwsd.IfThenElse (stageExpr0 e0) (stageExpr0 e1) (stageExpr0 e2)
  As e1 tye2 ->
    Lwsd.As (stageExpr0 e1) (stageTypeExpr0 tye2)
  LamOpt (x, tye1) e2 ->
    Lwsd.LamOpt (x, stageTypeExpr0 tye1) (stageExpr0 e2)
  AppOptGiven e1 e2 ->
    Lwsd.AppOptGiven (stageExpr0 e1) (stageExpr0 e2)
  AppOptOmitted e1 ->
    Lwsd.AppOptOmitted (stageExpr0 e1)

stageExpr1 :: BCExprF ann -> Lwsd.ExprF ann
stageExpr1 (Expr (btc, ann) exprMain) =
  case btc of
    BT0 -> Lwsd.Expr ann (Lwsd.Escape (Lwsd.Expr ann (stageExpr0Main exprMain)))
    BT1 -> Lwsd.Expr ann (stageExpr1Main exprMain)

stageExpr1Main :: BCExprMainF ann -> Lwsd.ExprMainF ann
stageExpr1Main = \case
  Literal lit ->
    Lwsd.Literal (convertLiteral stageExpr1 lit)
  Var x ->
    Lwsd.Var ([], x) -- TODO: fix this about module name chains
  Lam Nothing (x, tye1) e2 ->
    Lwsd.Lam Nothing (x, stageTypeExpr1 tye1) (stageExpr1 e2)
  Lam (Just (f, tyeRec)) (x, tye1) e2 ->
    Lwsd.Lam (Just (f, stageTypeExpr1 tyeRec)) (x, stageTypeExpr1 tye1) (stageExpr1 e2)
  App e1 e2 ->
    Lwsd.App (stageExpr1 e1) (stageExpr1 e2)
  LetIn x e1 e2 ->
    Lwsd.LetIn x (stageExpr1 e1) (stageExpr1 e2)
  IfThenElse e0 e1 e2 ->
    Lwsd.IfThenElse (stageExpr1 e0) (stageExpr1 e1) (stageExpr1 e2)
  As e1 tye2 ->
    Lwsd.As (stageExpr1 e1) (stageTypeExpr1 tye2)
  LamOpt (_x, _tye1) _e2 ->
    error "bug: stageExpr1Main, LamOpt"
  AppOptGiven _e1 _e2 ->
    error "bug: stageExpr1Main, AppOptGiven"
  AppOptOmitted _e1 ->
    error "bug: stageExpr1Main, AppOptOmitted"

stageTypeExpr0 :: BCTypeExprF ann -> Lwsd.TypeExprF ann
stageTypeExpr0 (TypeExpr (btc, ann) typeExprMain) =
  case btc of
    BT1 -> Lwsd.TypeExpr ann (Lwsd.TyCode (Lwsd.TypeExpr ann (stageTypeExpr1Main typeExprMain)))
    BT0 -> Lwsd.TypeExpr ann (stageTypeExpr0Main typeExprMain)

stageTypeExpr0Main :: BCTypeExprMainF ann -> Lwsd.TypeExprMainF ann
stageTypeExpr0Main = \case
  TyName tyName args ->
    case args of
      [] -> Lwsd.TyName tyName []
      _ : _ -> error "bug: stageTypeExpr0Main, non-empty `args`"
  TyArrow (xOpt, tye1) tye2 ->
    Lwsd.TyArrow (xOpt, stageTypeExpr0 tye1) (stageTypeExpr0 tye2)
  TyOptArrow (x, tye1) tye2 ->
    Lwsd.TyOptArrow (x, stageTypeExpr0 tye1) (stageTypeExpr0 tye2)

stageTypeExpr1 :: BCTypeExprF ann -> Lwsd.TypeExprF ann
stageTypeExpr1 (TypeExpr (btc, ann) typeExprMain) =
  case btc of
    BT0 -> error "bug: stageTypeExpr1, BT0"
    BT1 -> Lwsd.TypeExpr ann (stageTypeExpr1Main typeExprMain)

stageTypeExpr1Main :: BCTypeExprMainF ann -> Lwsd.TypeExprMainF ann
stageTypeExpr1Main = \case
  TyName tyName args -> Lwsd.TyName tyName (map stageArgForType1 args)
  TyArrow (xOpt, tye1) tye2 -> Lwsd.TyArrow (xOpt, stageTypeExpr1 tye1) (stageTypeExpr1 tye2)
  TyOptArrow (x, tye1) tye2 -> Lwsd.TyOptArrow (x, stageTypeExpr1 tye1) (stageTypeExpr1 tye2)

stageArgForType1 :: BCArgForTypeF ann -> Lwsd.ArgForTypeF ann
stageArgForType1 = \case
  ExprArg e -> Lwsd.ExprArgPersistent (stageExpr0 e)
  TypeArg tye -> Lwsd.TypeArg (stageTypeExpr1 tye)

convertLiteral :: (se -> le) -> Literal se -> Lwsd.Literal le
convertLiteral conv = \case
  LitInt n -> Lwsd.LitInt n
  LitList es -> Lwsd.LitList (map conv es)
  LitVec ns -> Lwsd.LitVec ns
  LitMat nss -> Lwsd.LitMat nss
