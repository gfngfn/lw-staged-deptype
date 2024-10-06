module Surface.Syntax
  ( Var,
    Literal (..),
    ExprF (..),
    ExprMain (..),
    Expr,
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

data ExprF ann = Expr ann (ExprMain ann)
  deriving stock (Show)

data ExprMain ann
  = Literal Literal
  | Var Var
  | Lam (Var, TypeExprF ann) (ExprF ann)
  | App (ExprF ann) (ExprF ann)
  | LetIn Var (ExprF ann) (ExprF ann)
  deriving stock (Show)

type Expr = ExprF Span

type TypeName = Text

data TypeExprF ann = TypeExpr ann (TypeExprMain ann)
  deriving stock (Show)

data TypeExprMain ann
  = TyName TypeName [ExprF ann]
  | TyArrow (Maybe Var, TypeExprF ann) (TypeExprF ann)
  deriving stock (Show)

type TypeExpr = TypeExprF Span
