module Syntax
  ( Var,
    Expr (..),
    AssExpr,
    TypeName,
    TypeExpr (..),
    AssTypeExpr,
  )
where

import Data.Text (Text)

type Var = Text

data Expr
  = Var Var
  | Lam (Var, TypeExpr) Expr
  | App Expr Expr
  deriving stock (Eq, Show)

-- TODO
type AssExpr = Expr

type TypeName = Text

data TypeExpr
  = TyName TypeName [Expr]
  | TyArrow (Maybe Var, TypeExpr) TypeExpr
  deriving stock (Eq, Show)

-- TODO
type AssTypeExpr = TypeExpr
