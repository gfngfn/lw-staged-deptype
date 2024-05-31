module Lwsd.Syntax
  ( Var,
    Symbol (..),
    symbolToVar,
    Literal (..),
    BuiltIn (..),
    ExprF (..),
    ExprMain (..),
    Expr,
    TypeName,
    TypeExprF (..),
    TypeExprMain (..),
    TypeExpr,
    ArgForTypeF (..),
    ArgForType,
    Ass0Expr (..),
    Ass1Expr (..),
    Type0Equation (..),
    decomposeType0Equation,
    Type1Equation (..),
    decomposeType1Equation,
    Ass0TypeExpr (..),
    Ass0PrimType (..),
    Ass1TypeExpr (..),
    Ass1PrimType (..),
    Ass0Val (..),
    Ass1Val (..),
    Ass1ValConst (..),
    Ass0TypeVal (..),
    Ass0PrimTypeVal (..),
    Ass1TypeVal (..),
    Ass1PrimTypeVal (..),
    Env0,
    EnvEntry (..),
  )
where

import Data.Functor.Classes
import Data.Map (Map)
import Data.Text (Text)
import Data.Text qualified as Text
import Generic.Data
import Generic.Data.Orphans ()
import Lwsd.Token (Span)
import Lwsd.Vector (Vector)

type Var = Text

-- The type for symbols generated dynamically for hygienicity.
newtype Symbol = Symbol Int
  deriving newtype (Eq, Show)

symbolToVar :: Symbol -> Var
symbolToVar (Symbol n) = Text.pack $ "#S" ++ show n

data Literal
  = LitInt Int
  | LitVec Vector
  deriving stock (Eq, Show)

data BuiltIn
  = BIAdd Var Var
  | BIGenVadd Var
  | BIGenVconcat Var Var
  | BIVadd Int Var Var
  | BIVconcat Int Int Var Var
  deriving stock (Eq, Show)

data ExprF ann = Expr ann (ExprMain ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 ExprF)

data ExprMain ann
  = Literal Literal
  | Var Var
  | Lam (Var, TypeExprF ann) (ExprF ann)
  | App (ExprF ann) (ExprF ann)
  | LetIn Var (ExprF ann) (ExprF ann)
  | Bracket (ExprF ann)
  | Escape (ExprF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 ExprMain)

-- The type for ASTs for expressions obtained by parsing source programs.
type Expr = ExprF Span

type TypeName = Text

data TypeExprF ann = TypeExpr ann (TypeExprMain ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 TypeExprF)

data TypeExprMain ann
  = TyName TypeName [ArgForTypeF ann]
  | TyArrow (Maybe Var, TypeExprF ann) (TypeExprF ann)
  | TyCode (TypeExprF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 TypeExprMain)

-- The type for ASTs for type expressions obtained by parsing source programs.
type TypeExpr = TypeExprF Span

data ArgForTypeF ann
  = PersistentArg (ExprF ann)
  | NormalArg (ExprF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 ArgForTypeF)

type ArgForType = ArgForTypeF Span

data Ass0Expr
  = A0Literal Literal
  | A0AppBuiltIn BuiltIn
  | A0Var Var
  | A0Lam (Var, Ass0TypeExpr) Ass0Expr
  | A0App Ass0Expr Ass0Expr
  | A0Bracket Ass1Expr
  | A0TyEqAssert Span Type0Equation Ass0Expr
  deriving stock (Eq, Show)

data Ass1Expr
  = A1Literal Literal
  | A1Var Var
  | A1Lam (Var, Ass1TypeExpr) Ass1Expr
  | A1App Ass1Expr Ass1Expr
  | A1Escape Ass0Expr
  deriving stock (Eq, Show)

data Ass0TypeExpr
  = A0TyPrim Ass0PrimType
  | A0TyArrow (Maybe Var, Ass0TypeExpr) Ass0TypeExpr
  | A0TyCode Ass1TypeExpr
  deriving stock (Eq, Show)

data Ass0PrimType
  = A0TyInt
  | A0TyBool
  | A0TyVec Int
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
  | A1ValConst Ass1ValConst
  | A1ValVar Symbol
  | A1ValLam (Symbol, Ass1TypeVal) Ass1Val
  | A1ValApp Ass1Val Ass1Val
  deriving stock (Eq, Show)

data Ass1ValConst
  = A1ValConstVadd Int
  | A1ValConstVconcat Int Int
  deriving stock (Eq, Show)

data Ass0TypeVal
  = A0TyValPrim Ass0PrimTypeVal
  | A0TyValArrow (Maybe Var, Ass0TypeVal) Ass0TypeExpr
  | A0TyValCode Ass1TypeVal
  deriving stock (Eq, Show)

data Ass0PrimTypeVal
  = A0TyValInt
  | A0TyValBool
  | A0TyValVec Int
  deriving stock (Eq, Show)

data Ass1TypeVal
  = A1TyValPrim Ass1PrimTypeVal
  | A1TyValArrow Ass1TypeVal Ass1TypeVal
  deriving stock (Eq, Show)

data Ass1PrimTypeVal
  = A1TyValInt
  | A1TyValBool
  | A1TyValVec Int
  deriving stock (Eq, Show)

data Type0Equation
  = TyEq0PrimInt
  | TyEq0PrimBool
  | TyEq0PrimVec Int
  | TyEq0Code Type1Equation
  | TyEq0Arrow (Maybe Var) Type0Equation Type0Equation
  deriving stock (Eq, Show)

data Type1Equation
  = TyEq1PrimInt
  | TyEq1PrimBool
  | TyEq1PrimVec Ass0Expr Ass0Expr
  | TyEq1Arrow Type1Equation Type1Equation
  deriving stock (Eq, Show)

type Env0 = Map Var EnvEntry

data EnvEntry
  = Ass0ValEntry Ass0Val
  | SymbolEntry Symbol
  deriving stock (Eq, Show)

decomposeType0Equation :: Type0Equation -> (Ass0TypeExpr, Ass0TypeExpr)
decomposeType0Equation = \case
  TyEq0PrimInt -> prims A0TyInt
  TyEq0PrimBool -> prims A0TyBool
  TyEq0PrimVec n -> prims (A0TyVec n)
  TyEq0Code ty1eq ->
    let (a1tye1, a1tye2) = decomposeType1Equation ty1eq
     in (A0TyCode a1tye1, A0TyCode a1tye2)
  TyEq0Arrow xOpt ty0eqDom ty0eqCod ->
    let (a0tye11, a0tye21) = decomposeType0Equation ty0eqDom
        (a0tye12, a0tye22) = decomposeType0Equation ty0eqCod
     in (A0TyArrow (xOpt, a0tye11) a0tye12, A0TyArrow (xOpt, a0tye21) a0tye22)
  where
    prims p = (A0TyPrim p, A0TyPrim p)

decomposeType1Equation :: Type1Equation -> (Ass1TypeExpr, Ass1TypeExpr)
decomposeType1Equation = \case
  TyEq1PrimInt -> prims A1TyInt
  TyEq1PrimBool -> prims A1TyBool
  TyEq1PrimVec a0e1 a0e2 -> (A1TyPrim (A1TyVec a0e1), A1TyPrim (A1TyVec a0e2))
  TyEq1Arrow ty1eqDom ty1eqCod ->
    let (a1tye11, a1tye21) = decomposeType1Equation ty1eqDom
        (a1tye12, a1tye22) = decomposeType1Equation ty1eqCod
     in (A1TyArrow a1tye11 a1tye12, A1TyArrow a1tye21 a1tye22)
  where
    prims p = (A1TyPrim p, A1TyPrim p)
