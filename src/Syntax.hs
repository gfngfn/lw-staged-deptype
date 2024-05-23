module Syntax
  ( Var,
    Expr (..),
    Ass0Expr (..),
    Ass1Expr (..),
    TypeName,
    TypeExpr (..),
    ArgForType (..),
    Ass0TypeExpr (..),
    Ass1TypeExpr (..),
  )
where

import Data.Text (Text)

type Var = Text

data Expr
  = Var Var
  | Lam (Var, TypeExpr) Expr
  | App Expr Expr
  | LetIn Var Expr Expr
  | Bracket Expr
  | Escape Expr
  deriving stock (Eq, Show)

data Ass0Expr
  = A0Var Var
  | A0Lam (Var, Ass0TypeExpr) Ass0Expr
  | A0App Ass0Expr Ass0Expr
  | A0Bracket Ass1Expr
  deriving stock (Eq, Show)

data Ass1Expr
  = A1Var Var
  | A1Lam (Var, Ass1TypeExpr) Ass1Expr
  | A1App Ass1Expr Ass1Expr
  | A1Escape Ass0Expr
  deriving stock (Eq, Show)

type TypeName = Text

data TypeExpr
  = TyName TypeName [ArgForType]
  | TyArrow (Maybe Var, TypeExpr) TypeExpr
  | TyCode TypeExpr
  deriving stock (Eq, Show)

data ArgForType
  = PersistentArg Expr
  | NormalArg Expr
  deriving stock (Eq, Show)

data Ass0TypeExpr
  = A0TyName TypeName [Ass0Expr]
  | A0TyArrow (Maybe Var, Ass0TypeExpr) Ass0TypeExpr
  | A0TyCode Ass1TypeExpr
  deriving stock (Eq, Show)

data Ass1TypeExpr
  = A1TyName TypeName [Ass0Expr]
  | A1TyArrow Ass1TypeExpr Ass1TypeExpr
  deriving stock (Eq, Show)
