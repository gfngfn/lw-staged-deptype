module Lwsd.Syntax
  ( Var,
    Symbol (..),
    symbolToVar,
    Literal (..),
    BuiltIn (..),
    ExprF (..),
    ExprMainF (..),
    Expr,
    ExprMain,
    TypeName,
    TypeExprF (..),
    TypeExprMainF (..),
    TypeExpr,
    TypeExprMain,
    ArgForTypeF (..),
    ArgForType,
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

import Data.Functor.Classes
import Data.Map (Map)
import Data.Text (Text)
import Data.Text qualified as Text
import Generic.Data
import Generic.Data.Orphans ()
import Util.Matrix (Matrix)
import Util.TokenUtil (Span)
import Util.Vector (Vector)
import Prelude

type Var = Text

-- The type for symbols generated dynamically for hygienicity.
newtype Symbol = Symbol Int
  deriving newtype (Eq, Show)

symbolToVar :: Symbol -> Var
symbolToVar (Symbol n) = Text.pack $ "#S" ++ show n

data Literal
  = LitInt Int
  | LitVec [Int]
  | LitMat [[Int]]
  deriving stock (Eq, Show)

data BuiltIn
  = BIAdd Var Var
  | BISub Var Var
  | BIMult Var Var
  | BIGenVadd Var
  | BIGenVconcat Var Var
  | BIGenMtranspose Var Var
  | BIGenMmult Var Var Var
  | BIGenMconcatVert Var Var Var
  | BIVadd Int Var Var
  | BIVconcat Int Int Var Var
  | BIMtranspose Int Int Var
  | BIMmult Int Int Int Var Var
  | BIMconcatVert Int Int Int Var Var
  deriving stock (Eq, Show)

data ExprF ann = Expr ann (ExprMainF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 ExprF)

data ExprMainF ann
  = Literal Literal
  | Var Var
  | Lam (Var, TypeExprF ann) (ExprF ann)
  | App (ExprF ann) (ExprF ann)
  | LetIn Var (ExprF ann) (ExprF ann)
  | Bracket (ExprF ann)
  | Escape (ExprF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 ExprMainF)

-- The type for ASTs for expressions obtained by parsing source programs.
type Expr = ExprF Span

type ExprMain = ExprMainF Span

type TypeName = Text

data TypeExprF ann = TypeExpr ann (TypeExprMainF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 TypeExprF)

data TypeExprMainF ann
  = TyName TypeName [ArgForTypeF ann]
  | TyArrow (Maybe Var, TypeExprF ann) (TypeExprF ann)
  | TyCode (TypeExprF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 TypeExprMainF)

-- The type for ASTs for type expressions obtained by parsing source programs.
type TypeExpr = TypeExprF Span

type TypeExprMain = TypeExprMainF Span

data ArgForTypeF ann
  = PersistentArg (ExprF ann)
  | NormalArg (ExprF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 ArgForTypeF)

type ArgForType = ArgForTypeF Span

data AssLiteral
  = ALitInt Int
  | ALitVec Vector
  | ALitMat Matrix
  deriving stock (Eq, Show)

data Ass0Expr
  = A0Literal AssLiteral
  | A0AppBuiltIn BuiltIn
  | A0Var Var
  | A0Lam (Var, Ass0TypeExpr) Ass0Expr
  | A0App Ass0Expr Ass0Expr
  | A0Bracket Ass1Expr
  | A0TyEqAssert Span Type1Equation
  deriving stock (Eq, Show)

data Ass1Expr
  = A1Literal AssLiteral
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
  | A0ValLam (Var, Ass0TypeVal) Ass0Expr Env0
  | A0ValBracket Ass1Val
  deriving stock (Eq, Show)

data Ass1Val
  = A1ValLiteral AssLiteral
  | A1ValConst Ass1ValConst
  | A1ValVar Symbol
  | A1ValLam (Symbol, Ass1TypeVal) Ass1Val
  | A1ValApp Ass1Val Ass1Val
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
  | A0TyValArrow (Maybe Var, Ass0TypeVal) Ass0TypeExpr
  | A0TyValCode Ass1TypeVal
  deriving stock (Eq, Show)

data Ass0PrimTypeVal
  = A0TyValInt
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

type Env0 = Map Var EnvEntry

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
