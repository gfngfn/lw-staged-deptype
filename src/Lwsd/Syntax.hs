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
    StrictAss0TypeExpr (..),
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
    a0TyVec,
    a0TyMat,
    a1TyVec,
    a1TyMat,
    mapAssLiteral,
    mapMAssLiteral,
    strictify,
    decomposeType1Equation,
    AppContext,
    AppContextEntry (..),
    Result (..),
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

data AssLiteral e
  = ALitInt Int
  | ALitBool Bool
  | ALitList [e]
  | ALitVec Vector
  | ALitMat Matrix
  deriving stock (Eq, Show)

data Ass0Expr
  = A0Literal (AssLiteral Ass0Expr)
  | A0AppBuiltIn BuiltIn
  | A0Var AssVar
  | A0Lam (Maybe (AssVar, StrictAss0TypeExpr)) (AssVar, StrictAss0TypeExpr) Ass0Expr
  | A0App Ass0Expr Ass0Expr
  | A0LetIn (AssVar, StrictAss0TypeExpr) Ass0Expr Ass0Expr
  | A0IfThenElse Ass0Expr Ass0Expr Ass0Expr
  | A0Bracket Ass1Expr
  | A0TyEqAssert Span Type1Equation
  deriving stock (Eq, Show)

data Ass1Expr
  = A1Literal (AssLiteral Ass1Expr)
  | A1Var AssVar
  | A1Lam (Maybe (AssVar, Ass1TypeExpr)) (AssVar, Ass1TypeExpr) Ass1Expr
  | A1App Ass1Expr Ass1Expr
  | A1IfThenElse Ass1Expr Ass1Expr Ass1Expr
  | A1Escape Ass0Expr
  deriving stock (Eq, Show)

-- For type-checking.
data Ass0TypeExpr
  = A0TyPrim Ass0PrimType
  | A0TyList Ass0TypeExpr
  | A0TyArrow (Maybe AssVar, Ass0TypeExpr) Ass0TypeExpr
  | A0TyOptArrow (AssVar, Ass0TypeExpr) Ass0TypeExpr
  | A0TyCode Ass1TypeExpr
  deriving stock (Eq, Show)

-- For type annotations in target terms.
data StrictAss0TypeExpr
  = SA0TyPrim Ass0PrimType
  | SA0TyList StrictAss0TypeExpr
  | SA0TyArrow (Maybe AssVar, StrictAss0TypeExpr) StrictAss0TypeExpr
  | SA0TyCode Ass1TypeExpr
  deriving stock (Eq, Show)

data Ass0PrimType
  = A0TyInt
  | A0TyNat
  | A0TyBool
  | A0TyTensor [Int]
  deriving stock (Eq, Show)

data Ass1TypeExpr
  = A1TyPrim Ass1PrimType
  | A1TyList Ass1TypeExpr
  | A1TyArrow Ass1TypeExpr Ass1TypeExpr
  deriving stock (Eq, Show)

data Ass1PrimType
  = A1TyInt
  | A1TyBool
  | A1TyTensor [Ass0Expr]
  deriving stock (Eq, Show)

data Ass0Val
  = A0ValLiteral (AssLiteral Ass0Val)
  | A0ValLam (Maybe (AssVar, Ass0TypeVal)) (AssVar, Ass0TypeVal) Ass0Expr Env0
  | A0ValBracket Ass1Val
  deriving stock (Eq, Show)

data Ass1Val
  = A1ValLiteral (AssLiteral Ass1Val)
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
  | A0TyValList Ass0TypeVal
  | A0TyValArrow (Maybe AssVar, Ass0TypeVal) StrictAss0TypeExpr
  | A0TyValCode Ass1TypeVal
  deriving stock (Eq, Show)

data Ass0PrimTypeVal
  = A0TyValInt
  | A0TyValNat
  | A0TyValBool
  | A0TyValTensor [Int]
  deriving stock (Eq, Show)

data Ass1TypeVal
  = A1TyValPrim Ass1PrimTypeVal
  | A1TyValList Ass1TypeVal
  | A1TyValArrow Ass1TypeVal Ass1TypeVal
  deriving stock (Eq, Show)

data Ass1PrimTypeVal
  = A1TyValInt
  | A1TyValBool
  | A1TyValTensor [Int]
  deriving stock (Eq, Show)

data Type1Equation
  = TyEq1Prim Type1PrimEquation
  | TyEq1List Type1Equation
  | TyEq1Arrow Type1Equation Type1Equation
  deriving stock (Eq, Show)

data Type1PrimEquation
  = TyEq1Int
  | TyEq1Bool
  | TyEq1Tensor [(Ass0Expr, Ass0Expr)]
  deriving stock (Eq, Show)

type Env0 = Map AssVar EnvEntry

data EnvEntry
  = Ass0ValEntry Ass0Val
  | SymbolEntry Symbol
  deriving stock (Eq, Show)

mapAssLiteral :: (e1 -> e2) -> AssLiteral e1 -> AssLiteral e2
mapAssLiteral f = \case
  ALitInt n -> ALitInt n
  ALitBool b -> ALitBool b
  ALitList es -> ALitList (map f es)
  ALitVec vec -> ALitVec vec
  ALitMat mat -> ALitMat mat

mapMAssLiteral :: (Monad m) => (e -> m v) -> AssLiteral e -> m (AssLiteral v)
mapMAssLiteral eval = \case
  ALitInt n -> pure $ ALitInt n
  ALitBool b -> pure $ ALitBool b
  ALitList a0es -> ALitList <$> mapM eval a0es
  ALitVec vec -> pure $ ALitVec vec
  ALitMat mat -> pure $ ALitMat mat

strictify :: Ass0TypeExpr -> StrictAss0TypeExpr
strictify = \case
  A0TyPrim a0tyPrim -> SA0TyPrim a0tyPrim
  A0TyList a0tye -> SA0TyList (strictify a0tye)
  A0TyArrow (x1opt, a0tye1) a0tye2 -> SA0TyArrow (x1opt, strictify a0tye1) (strictify a0tye2)
  A0TyCode a1tye1 -> SA0TyCode a1tye1
  A0TyOptArrow (x1, a0tye1) a0tye2 -> SA0TyArrow (Just x1, strictify a0tye1) (strictify a0tye2)

a0TyVec :: Int -> Ass0PrimType
a0TyVec n = A0TyTensor [n]

a0TyMat :: Int -> Int -> Ass0PrimType
a0TyMat m n = A0TyTensor [m, n]

a1TyVec :: Ass0Expr -> Ass1PrimType
a1TyVec a0e = A1TyTensor [a0e]

a1TyMat :: Ass0Expr -> Ass0Expr -> Ass1PrimType
a1TyMat a0e1 a0e2 = A1TyTensor [a0e1, a0e2]

decomposeType1Equation :: Type1Equation -> (Ass1TypeExpr, Ass1TypeExpr)
decomposeType1Equation = \case
  TyEq1Prim ty1eqPrim ->
    case ty1eqPrim of
      TyEq1Int -> prims A1TyInt
      TyEq1Bool -> prims A1TyBool
      TyEq1Tensor zipped -> (A1TyPrim (A1TyTensor (map fst zipped)), A1TyPrim (A1TyTensor (map snd zipped)))
  TyEq1List ty1eqElem ->
    let (a1tye1elem, a1tye2elem) = decomposeType1Equation ty1eqElem
     in (A1TyList a1tye1elem, A1TyList a1tye2elem)
  TyEq1Arrow ty1eqDom ty1eqCod ->
    let (a1tye11, a1tye21) = decomposeType1Equation ty1eqDom
        (a1tye12, a1tye22) = decomposeType1Equation ty1eqCod
     in (A1TyArrow a1tye11 a1tye12, A1TyArrow a1tye21 a1tye22)
  where
    prims p = (A1TyPrim p, A1TyPrim p)

type AppContext = [AppContextEntry]

data AppContextEntry
  = AppArg0 Ass0Expr Ass0TypeExpr
  | AppArg1 Ass1TypeExpr
  | AppArgOptGiven0 Ass0Expr Ass0TypeExpr
  | AppArgOptOmitted0
  deriving (Eq, Show)

data Result a
  = Pure a
  | Cast0 (Maybe Ass0Expr) Ass0TypeExpr (Result a)
  | Cast1 (Maybe Ass0Expr) Ass1TypeExpr (Result a)
  | CastGiven0 (Maybe Ass0Expr) Ass0TypeExpr (Result a)
  | FillInferred0 Ass0Expr (Result a)
  | InsertInferred0 Ass0Expr (Result a)
  deriving (Eq, Show)
