module Syntax
  ( Var,
    Literal (..),
    Expr (..),
    Ass0Expr (..),
    Ass1Expr (..),
    TypeName,
    TypeExpr (..),
    ArgForType (..),
    Ass0TypeExpr (..),
    Ass0PrimType (..),
    Ass1TypeExpr (..),
    Ass1PrimType (..),
    Ass0Val (..),
    Ass1Val (..),
    Ass0TypeVal (..),
    Ass0PrimTypeVal (..),
    Ass1TypeVal (..),
    Ass1PrimTypeVal (..),
    Env0,
  )
where

import Data.Map (Map)
import Data.Text (Text)

type Var = Text

data Literal
  = LitInt Int
  deriving stock (Eq, Show)

data Expr
  = Literal Literal
  | Var Var
  | Lam (Var, TypeExpr) Expr
  | App Expr Expr
  | LetIn Var Expr Expr
  | Bracket Expr
  | Escape Expr
  deriving stock (Eq, Show)

data Ass0Expr
  = A0Literal Literal
  | A0Var Var
  | A0Lam (Var, Ass0TypeExpr) Ass0Expr
  | A0App Ass0Expr Ass0Expr
  | A0Bracket Ass1Expr
  | A0AssertAndThen Ass0Expr Ass0Expr Ass0Expr
  deriving stock (Eq, Show)

data Ass1Expr
  = A1Literal Literal
  | A1Var Var
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
  = A0TyPrim Ass0PrimType
  | A0TyArrow (Maybe Var, Ass0TypeExpr) Ass0TypeExpr
  | A0TyCode Ass1TypeExpr
  deriving stock (Eq, Show)

data Ass0PrimType
  = A0TyInt
  | A0TyBool
  deriving stock (Eq, Show)

data Ass1TypeExpr
  = A1TyPrim Ass1PrimType
  | A1TyArrow Ass1TypeExpr Ass1TypeExpr
  deriving stock (Eq, Show)

data Ass1PrimType
  = A1TyInt
  | A1TyBool
  | A1TyVec Ass0Expr
  deriving stock (Eq, Show)

data Ass0Val
  = A0ValLiteral Literal
  | A0ValLam (Var, Ass0TypeVal) Ass0Expr Env0
  | A0ValBracket Ass1Val
  deriving stock (Eq, Show)

data Ass1Val
  = A1ValLiteral Literal
  | A1ValVar Var
  | A1ValLam (Var, Ass1TypeVal) Ass1Val
  | A1ValApp Ass1Val Ass1Val
  deriving stock (Eq, Show)

data Ass0TypeVal
  = A0TyValPrim Ass0PrimTypeVal
  | A0TyValArrow (Maybe Var, Ass0TypeVal) Ass0TypeExpr
  | A0TyValCode Ass1TypeVal
  deriving stock (Eq, Show)

data Ass0PrimTypeVal
  = A0TyValInt
  | A0TyValBool
  deriving stock (Eq, Show)

data Ass1TypeVal
  = A1TyValPrim Ass1PrimTypeVal
  | A1TyValArrow Ass1TypeVal Ass1TypeVal
  deriving stock (Eq, Show)

data Ass1PrimTypeVal
  = A1TyValInt
  | A1TyValBool
  | A1TyValVec Ass0Val
  deriving stock (Eq, Show)

type Env0 = Map Var Ass0Val
