module Surface.Syntax
  ( ExprF (..),
    ExprMain (..),
    TypeExprF (..),
    TypeExprMain (..),
  )
where

import Data.Text (Text)

type Var = Text

data ExprF ann = Expr ann (ExprMain ann)

data Literal
  = LitInt Int
  | LitVec [Int]
  | LitMat [[Int]]
  deriving stock (Eq, Show)

data ExprMain ann
  = Literal Literal
  | Var Var
  | Lam (Var, TypeExprF ann) (ExprF ann)
  | App (ExprF ann) (ExprF ann)
  | LetIn Var (ExprF ann) (ExprF ann)

type TypeName = Text

data TypeExprF ann = TypeExpr ann (TypeExprMain ann)

data TypeExprMain ann
  = TyName TypeName [ExprF ann]
  | TyArrow (Maybe Var, TypeExprF ann) (TypeExprF ann)
