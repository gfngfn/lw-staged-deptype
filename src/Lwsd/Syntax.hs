module Lwsd.Syntax
  ( AssVar (..),
    Symbol (..),
    symbolToVar,
    BuiltIn (..),
    AssLiteral (..),
    Ass0Expr (..),
    Ass1Expr (..),
    Type1Equation (..),
    Type1PrimEquation (..),
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
    decomposeType1Equation,
  )
where

import Data.Map (Map)
import Data.Text (Text)
import Data.Text qualified as Text
import Util.Matrix (Matrix)
import Util.TokenUtil (Span)
import Util.Vector (Vector)
import Prelude

newtype AssVar = AssVar Text
  deriving newtype (Eq, Ord)

instance Show AssVar where
  show (AssVar x) = show x

-- The type for symbols generated dynamically for hygienicity.
newtype Symbol = Symbol Int
  deriving newtype (Eq, Show)

symbolToVar :: Symbol -> AssVar
symbolToVar (Symbol n) = AssVar $ Text.pack $ "#S" ++ show n

data BuiltIn
  = BIAdd AssVar AssVar
  | BISub AssVar AssVar
  | BIMult AssVar AssVar
  | BILeq AssVar AssVar
  | BIAssertNat Span AssVar
  | BIGenVadd AssVar
  | BIGenVconcat AssVar AssVar
  | BIGenMtranspose AssVar AssVar
  | BIGenMmult AssVar AssVar AssVar
  | BIGenMconcatVert AssVar AssVar AssVar
  | BIVadd Int AssVar AssVar
  | BIVconcat Int Int AssVar AssVar
  | BIMtranspose Int Int AssVar
  | BIMmult Int Int Int AssVar AssVar
  | BIMconcatVert Int Int Int AssVar AssVar
  deriving stock (Eq, Show)

data AssLiteral
  = ALitInt Int
  | ALitBool Bool
  | ALitVec Vector
  | ALitMat Matrix
  deriving stock (Eq, Show)

data Ass0Expr
  = A0Literal AssLiteral
  | A0AppBuiltIn BuiltIn
  | A0Var AssVar
  | A0Lam (Maybe (AssVar, Ass0TypeExpr)) (AssVar, Ass0TypeExpr) Ass0Expr
  | A0App Ass0Expr Ass0Expr
  | A0IfThenElse Ass0Expr Ass0Expr Ass0Expr
  | A0Bracket Ass1Expr
  | A0TyEqAssert Span Type1Equation
  deriving stock (Eq, Show)

data Ass1Expr
  = A1Literal AssLiteral
  | A1Var AssVar
  | A1Lam (Maybe (AssVar, Ass1TypeExpr)) (AssVar, Ass1TypeExpr) Ass1Expr
  | A1App Ass1Expr Ass1Expr
  | A1IfThenElse Ass1Expr Ass1Expr Ass1Expr
  | A1Escape Ass0Expr
  deriving stock (Eq, Show)

data Ass0TypeExpr
  = A0TyPrim Ass0PrimType
  | A0TyArrow (Maybe AssVar, Ass0TypeExpr) Ass0TypeExpr
  | A0TyOptArrow (AssVar, Ass0TypeExpr) Ass0TypeExpr
  | A0TyCode Ass1TypeExpr
  deriving stock (Eq, Show)

data Ass0PrimType
  = A0TyInt
  | A0TyNat
  | A0TyBool
  | A0TyVec Int
  | A0TyMat Int Int
  deriving stock (Eq, Show)

data Ass1TypeExpr
  = A1TyPrim Ass1PrimType
  | A1TyArrow Ass1TypeExpr Ass1TypeExpr
  deriving stock (Eq, Show)

data Ass1PrimType
  = A1TyInt
  | A1TyBool
  | A1TyVec Ass0Expr
  | A1TyMat Ass0Expr Ass0Expr
  deriving stock (Eq, Show)

data Ass0Val
  = A0ValLiteral AssLiteral
  | A0ValLam (Maybe (AssVar, Ass0TypeVal)) (AssVar, Ass0TypeVal) Ass0Expr Env0
  | A0ValBracket Ass1Val
  deriving stock (Eq, Show)

data Ass1Val
  = A1ValLiteral AssLiteral
  | A1ValConst Ass1ValConst
  | A1ValVar Symbol
  | A1ValLam (Maybe (Symbol, Ass1TypeVal)) (Symbol, Ass1TypeVal) Ass1Val
  | A1ValApp Ass1Val Ass1Val
  | A1ValIfThenElse Ass1Val Ass1Val Ass1Val
  deriving stock (Eq, Show)

data Ass1ValConst
  = A1ValConstVadd Int
  | A1ValConstVconcat Int Int
  | A1ValConstMtranspose Int Int
  | A1ValConstMmult Int Int Int
  | A1ValConstMconcatVert Int Int Int
  deriving stock (Eq, Show)

data Ass0TypeVal
  = A0TyValPrim Ass0PrimTypeVal
  | A0TyValArrow (Maybe AssVar, Ass0TypeVal) Ass0TypeExpr
  | A0TyValCode Ass1TypeVal
  deriving stock (Eq, Show)

data Ass0PrimTypeVal
  = A0TyValInt
  | A0TyValNat
  | A0TyValBool
  | A0TyValVec Int
  | A0TyValMat Int Int
  deriving stock (Eq, Show)

data Ass1TypeVal
  = A1TyValPrim Ass1PrimTypeVal
  | A1TyValArrow Ass1TypeVal Ass1TypeVal
  deriving stock (Eq, Show)

data Ass1PrimTypeVal
  = A1TyValInt
  | A1TyValBool
  | A1TyValVec Int
  | A1TyValMat Int Int
  deriving stock (Eq, Show)

data Type1Equation
  = TyEq1Prim Type1PrimEquation
  | TyEq1Arrow Type1Equation Type1Equation
  deriving stock (Eq, Show)

data Type1PrimEquation
  = TyEq1Int
  | TyEq1Bool
  | TyEq1Vec Ass0Expr Ass0Expr
  | TyEq1Mat Ass0Expr Ass0Expr Ass0Expr Ass0Expr
  deriving stock (Eq, Show)

type Env0 = Map AssVar EnvEntry

data EnvEntry
  = Ass0ValEntry Ass0Val
  | SymbolEntry Symbol
  deriving stock (Eq, Show)

decomposeType1Equation :: Type1Equation -> (Ass1TypeExpr, Ass1TypeExpr)
decomposeType1Equation = \case
  TyEq1Prim ty1eqPrim ->
    case ty1eqPrim of
      TyEq1Int -> prims A1TyInt
      TyEq1Bool -> prims A1TyBool
      TyEq1Vec a0e1 a0e2 -> (A1TyPrim (A1TyVec a0e1), A1TyPrim (A1TyVec a0e2))
      TyEq1Mat a0e11 a0e12 a0e21 a0e22 -> (A1TyPrim (A1TyMat a0e11 a0e12), A1TyPrim (A1TyMat a0e21 a0e22))
  TyEq1Arrow ty1eqDom ty1eqCod ->
    let (a1tye11, a1tye21) = decomposeType1Equation ty1eqDom
        (a1tye12, a1tye22) = decomposeType1Equation ty1eqCod
     in (A1TyArrow a1tye11 a1tye12, A1TyArrow a1tye21 a1tye22)
  where
    prims p = (A1TyPrim p, A1TyPrim p)
