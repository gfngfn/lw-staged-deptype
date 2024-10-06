module Surface.Syntax
  ( Var,
    Literal (..),
    ExprF (..),
    ExprMainF (..),
    Expr,
    TypeName,
    TypeExprF (..),
    TypeExprMain (..),
    TypeExpr,
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
  | Lam (Var, TypeExprF ann) (ExprF ann)
  | App (ExprF ann) (ExprF ann)
  | LetIn Var (ExprF ann) (ExprF ann)
  deriving stock (Show, Functor)

type Expr = ExprF Span

type TypeName = Text

data TypeExprF ann = TypeExpr ann (TypeExprMain ann)
  deriving stock (Show, Functor)

data TypeExprMain ann
  = TyName TypeName [ExprF ann]
  | TyArrow (Maybe Var, TypeExprF ann) (TypeExprF ann)
  deriving stock (Show, Functor)

type TypeExpr = TypeExprF Span
