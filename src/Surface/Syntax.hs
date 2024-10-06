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

data ExprMain ann
  = Literal Literal
  | Var Var
  | Lam (Var, TypeExprF ann) (ExprF ann)
  | App (ExprF ann) (ExprF ann)
  | LetIn Var (ExprF ann) (ExprF ann)

type Expr = ExprF Span

type TypeName = Text

data TypeExprF ann = TypeExpr ann (TypeExprMain ann)

data TypeExprMain ann
  = TyName TypeName [ExprF ann]
  | TyArrow (Maybe Var, TypeExprF ann) (TypeExprF ann)

type TypeExpr = TypeExprF Span
