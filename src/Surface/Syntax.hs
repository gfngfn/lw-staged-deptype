module Surface.Syntax
  ( Var,
    Literal (..),
    ExprF (..),
    ExprMainF (..),
    Expr,
    ExprMain,
    TypeName,
    TypeExprF (..),
    TypeExprMainF (..),
    TypeExpr,
    TypeExprMain,
  )
where

import Data.Text (Text)
import Util.TokenUtil (Span)
import Prelude

type Var = Text

data Literal
  = LitInt Int
  | LitVec [Int]
  | LitMat [[Int]]
  deriving stock (Eq, Show)

data ExprF ann = Expr ann (ExprMainF ann)
  deriving stock (Show, Functor)

data ExprMainF ann
  = Literal Literal
  | Var Var
  | Lam (Maybe (Var, TypeExprF ann)) (Var, TypeExprF ann) (ExprF ann)
  | App (ExprF ann) (ExprF ann)
  | LetIn Var (ExprF ann) (ExprF ann)
  | IfThenElse (ExprF ann) (ExprF ann) (ExprF ann)
  | As (ExprF ann) (TypeExprF ann)
  | LamOpt (Var, TypeExprF ann) (ExprF ann)
  | AppOptGiven (ExprF ann) (ExprF ann)
  | AppOptOmitted (ExprF ann)
  deriving stock (Show, Functor)

type Expr = ExprF Span

type ExprMain = ExprMainF Span

type TypeName = Text

data TypeExprF ann = TypeExpr ann (TypeExprMainF ann)
  deriving stock (Show, Functor)

data TypeExprMainF ann
  = TyName TypeName [ExprF ann]
  | TyArrow (Maybe Var, TypeExprF ann) (TypeExprF ann)
  | TyOptArrow (Var, TypeExprF ann) (TypeExprF ann)
  deriving stock (Show, Functor)

type TypeExpr = TypeExprF Span

type TypeExprMain = TypeExprMainF Span
