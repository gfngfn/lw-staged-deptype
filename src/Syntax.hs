module Syntax
  ( Var,
    Expr (..),
    AssExpr0,
    AssExpr1,
    TypeName,
    TypeExpr (..),
    AssTypeExpr0,
    AssTypeExpr1,
  )
where

import Data.Text (Text)

type Var = Text

data Expr
  = Var Var
  | Lam (Var, TypeExpr) Expr
  | App Expr Expr
  | Bracket Expr
  | Escape Expr
  deriving stock (Eq, Show)

-- TODO
type AssExpr0 = Expr

-- TODO
type AssExpr1 = Expr

type TypeName = Text

data TypeExpr
  = TyName TypeName [Expr]
  | TyArrow (Maybe Var, TypeExpr) TypeExpr
  | TyCode TypeExpr
  deriving stock (Eq, Show)

-- TODO
type AssTypeExpr0 = TypeExpr

-- TODO
type AssTypeExpr1 = TypeExpr
