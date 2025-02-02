module Lwsd.Syntax
  ( AssVar (..),
    Symbol (..),
    symbolToVar,
    BuiltIn (..),
    BuiltInArity1 (..),
    BuiltInArity2 (..),
    BuiltInArity3 (..),
    Ass1BuiltInName (..),
    Ass0PartialBuiltInApp (..),
    unliftBuiltInName,
    AssLiteral (..),
    Ass0Expr (..),
    Ass1Expr (..),
    AssBind (..),
    makeExprFromBinds,
    Type1Equation (..),
    Type1PrimEquation (..),
    Ass0TypeExpr (..),
    StrictAss0TypeExpr (..),
    Ass0PrimType (..),
    Ass1TypeExpr (..),
    Ass1PrimType (..),
    AssPersTypeExpr (..),
    persistentTypeTo0,
    persistentTypeTo1,
    liftPrimType,
    Ass0Val (..),
    Ass1Val (..),
    Ass1ValConst (..),
    Ass0TypeVal (..),
    Ass0PrimTypeVal (..),
    Ass1TypeVal (..),
    Ass1PrimTypeVal (..),
    EvalEnv,
    EvalEnvEntry (..),
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
  = BuiltInArity1 BuiltInArity1
  | BuiltInArity2 BuiltInArity2
  | BuiltInArity3 BuiltInArity3
  deriving stock (Eq, Show)

data BuiltInArity1
  = BIGenVadd
  | BIMtranspose Int Int
  | BITensorGenZeros
  | BITensorGenGrad
  | BITensorGenZeroGrad
  | BITensorGenSubUpdate
  | BITensorGenCountEqual
  deriving stock (Eq, Show)

data BuiltInArity2
  = BIAdd
  | BISub
  | BIMult
  | BILeq
  | BIAnd
  | BIListMap
  | BIGenVconcat
  | BIGenMtranspose
  | BIVadd Int
  | BIVconcat Int Int
  | BIMconcatVert Int Int Int
  | BIDropAt
  | BIBroadcastable
  | BIBroadcast
  | BIListAppend
  | BIListIter
  | BIGenBroadcasted
  | BITensorGenAdd
  | BITensorGenMult
  | BITensorGenCrossEntropyForLogits
  | BITensorGenArgmax
  | BITensorAdd [Int]
  | BITensorMm Int Int Int
  deriving stock (Eq, Show)

data BuiltInArity3
  = BIGenMconcatVert
  | BITensorGenMm
  deriving stock (Eq, Show)

data Ass0PartialBuiltInApp
  = A0PartialBuiltInApp1With0 BuiltInArity1
  | A0PartialBuiltInApp2With0 BuiltInArity2
  | A0PartialBuiltInApp2With1 BuiltInArity2 Ass0Val
  | A0PartialBuiltInApp3With0 BuiltInArity3
  | A0PartialBuiltInApp3With1 BuiltInArity3 Ass0Val
  | A0PartialBuiltInApp3With2 BuiltInArity3 Ass0Val Ass0Val
  deriving stock (Eq, Show)

data Ass1BuiltInName
  = A1BINameAdd
  | A1BINameSub
  | A1BINameMult
  | A1BINameFloatDiv
  | A1BINameLeq
  | A1BINameFloat
  | A1BINamePrintFloat
  | A1BINameListAppend
  | A1BINameListIter
  | A1BINameRange
  | A1BINameTensorF
  | A1BINameTensorBackward
  | A1BINameTensorNoGrad
  | A1BINameTensorFloatValue
  | A1BINameMnistHelperTrainImages
  | A1BINameMnistHelperTrainLabels
  | A1BINameMnistHelperTestImages
  | A1BINameMnistHelperTestLabels
  deriving stock (Eq, Show)

unliftBuiltInName :: Ass1BuiltInName -> BuiltIn
unliftBuiltInName = \case
  A1BINameAdd -> arity2 BIAdd
  A1BINameSub -> arity2 BISub
  A1BINameMult -> arity2 BIMult
  A1BINameLeq -> arity2 BILeq
  A1BINameListAppend -> arity2 BIListAppend
  A1BINameListIter -> arity2 BIListIter
  a1builtInName -> error $ "TODO: unliftBuiltInName, " ++ show a1builtInName
  where
    arity2 = BuiltInArity2

data AssLiteral e
  = ALitInt Int
  | ALitFloat Double
  | ALitBool Bool
  | ALitUnit
  | ALitList [e]
  | ALitVec Vector
  | ALitMat Matrix
  deriving stock (Eq, Show)

data Ass0Expr
  = A0Literal (AssLiteral Ass0Expr)
  | A0BuiltInName BuiltIn
  | A0Var AssVar
  | A0Lam (Maybe (AssVar, StrictAss0TypeExpr)) (AssVar, StrictAss0TypeExpr) Ass0Expr
  | A0App Ass0Expr Ass0Expr
  | A0LetIn (AssVar, StrictAss0TypeExpr) Ass0Expr Ass0Expr
  | A0Sequential Ass0Expr Ass0Expr
  | A0IfThenElse Ass0Expr Ass0Expr Ass0Expr
  | A0Bracket Ass1Expr
  | A0TyEqAssert Span Type1Equation
  | A0RefinementAssert Span Ass0Expr Ass0Expr -- A predicate and a target.
  deriving stock (Eq, Show)

data Ass1Expr
  = A1Literal (AssLiteral Ass1Expr)
  | A1Var AssVar
  | A1BuiltInName Ass1BuiltInName
  | A1Lam (Maybe (AssVar, Ass1TypeExpr)) (AssVar, Ass1TypeExpr) Ass1Expr
  | A1App Ass1Expr Ass1Expr
  | A1Sequential Ass1Expr Ass1Expr
  | A1IfThenElse Ass1Expr Ass1Expr Ass1Expr
  | A1Escape Ass0Expr
  deriving stock (Eq, Show)

a1LetIn :: (AssVar, Ass1TypeExpr) -> Ass1Expr -> Ass1Expr -> Ass1Expr
a1LetIn (x, a1tye) a1e1 a1e2 =
  A1App (A1Lam Nothing (x, a1tye) a1e2) a1e1

data AssBind
  = ABind0 (AssVar, StrictAss0TypeExpr) Ass0Expr
  | ABind1 (AssVar, Ass1TypeExpr) Ass1Expr

makeExprFromBinds :: [AssBind] -> Ass0Expr -> Ass0Expr
makeExprFromBinds abinds' a0eFinal = go0 abinds'
  where
    go0 :: [AssBind] -> Ass0Expr
    go0 = \case
      [] -> a0eFinal
      ABind0 xsty a0e : abinds -> A0LetIn xsty a0e (go0 abinds)
      ABind1 xty a1e : abinds -> A0Bracket (a1LetIn xty a1e (go1 abinds))

    go1 :: [AssBind] -> Ass1Expr
    go1 = \case
      [] -> A1Escape a0eFinal
      ABind1 xty a1e : abinds -> a1LetIn xty a1e (go1 abinds)
      ABind0 xsty a0e : abinds -> A1Escape (A0LetIn xsty a0e (go0 abinds))

-- For type-checking.
data Ass0TypeExpr
  = A0TyPrim Ass0PrimType (Maybe Ass0Expr) -- Possibly equipped with a refinement predicate.
  | A0TyList Ass0TypeExpr (Maybe Ass0Expr) -- Possibly equipped with a refinement predicate.
  | A0TyArrow (Maybe AssVar, Ass0TypeExpr) Ass0TypeExpr
  | A0TyOptArrow (AssVar, Ass0TypeExpr) Ass0TypeExpr
  | A0TyCode Ass1TypeExpr
  deriving stock (Eq, Show)

-- For type annotations in target terms.
data StrictAss0TypeExpr
  = SA0TyPrim Ass0PrimType (Maybe Ass0Expr) -- Possibly equipped with a refinement predicate.
  | SA0TyList StrictAss0TypeExpr (Maybe Ass0Expr) -- Possibly equipped with a refinement predicate.
  | SA0TyArrow (Maybe AssVar, StrictAss0TypeExpr) StrictAss0TypeExpr
  | SA0TyCode Ass1TypeExpr
  deriving stock (Eq, Show)

data Ass0PrimType
  = A0TyInt
  | A0TyFloat
  | A0TyBool
  | A0TyUnit
  | A0TyTensor [Int]
  deriving stock (Eq, Show)

data Ass1TypeExpr
  = A1TyPrim Ass1PrimType
  | A1TyList Ass1TypeExpr
  | A1TyArrow Ass1TypeExpr Ass1TypeExpr
  deriving stock (Eq, Show)

data Ass1PrimType
  = A1TyInt
  | A1TyFloat
  | A1TyBool
  | A1TyUnit
  | A1TyTensor Ass0Expr
  deriving stock (Eq, Show)

-- Types for persistent value items.
data AssPersTypeExpr
  = APersTyPrim Ass0PrimType
  | APersTyList AssPersTypeExpr
  | APersTyArrow AssPersTypeExpr AssPersTypeExpr
  deriving stock (Eq, Show)

persistentTypeTo0 :: AssPersTypeExpr -> Ass0TypeExpr
persistentTypeTo0 = \case
  APersTyPrim a0tyPrim -> A0TyPrim a0tyPrim Nothing
  APersTyList aPtye -> A0TyList (persistentTypeTo0 aPtye) Nothing
  APersTyArrow aPtye1 aPtye2 -> A0TyArrow (Nothing, persistentTypeTo0 aPtye1) (persistentTypeTo0 aPtye2)

persistentTypeTo1 :: AssPersTypeExpr -> Ass1TypeExpr
persistentTypeTo1 = \case
  APersTyPrim a0tyPrim -> A1TyPrim (liftPrimType a0tyPrim)
  APersTyList aPtye -> A1TyList (persistentTypeTo1 aPtye)
  APersTyArrow aPtye1 aPtye2 -> A1TyArrow (persistentTypeTo1 aPtye1) (persistentTypeTo1 aPtye2)

liftPrimType :: Ass0PrimType -> Ass1PrimType
liftPrimType = \case
  A0TyInt -> A1TyInt
  A0TyFloat -> A1TyFloat
  A0TyBool -> A1TyBool
  A0TyUnit -> A1TyUnit
  A0TyTensor ns -> A1TyTensor (A0Literal (ALitList (map (A0Literal . ALitInt) ns)))

data Ass0Val
  = A0ValLiteral (AssLiteral Ass0Val)
  | A0ValLam (Maybe (AssVar, Ass0TypeVal)) (AssVar, Ass0TypeVal) Ass0Expr EvalEnv
  | A0ValBracket Ass1Val
  | A0ValPartialBuiltInApp Ass0PartialBuiltInApp
  deriving stock (Eq, Show)

data Ass1Val
  = A1ValLiteral (AssLiteral Ass1Val)
  | A1ValConst Ass1ValConst
  | A1ValVar Symbol
  | A1ValLam (Maybe (Symbol, Ass1TypeVal)) (Symbol, Ass1TypeVal) Ass1Val
  | A1ValApp Ass1Val Ass1Val
  | A1ValSequential Ass1Val Ass1Val
  | A1ValIfThenElse Ass1Val Ass1Val Ass1Val
  deriving stock (Eq, Show)

data Ass1ValConst
  = A1ValConstVadd Int
  | A1ValConstVconcat Int Int
  | A1ValConstMtranspose Int Int
  | A1ValConstMconcatVert Int Int Int
  | A1ValConstBroadcasted [Int] [Int]
  | A1ValConstTensorZeros [Int]
  | A1ValConstTensorAdd [Int] [Int]
  | A1ValConstTensorMult [Int] [Int]
  | A1ValConstTensorMm Int Int Int
  | A1ValConstTensorGrad [Int]
  | A1ValConstTensorZeroGrad [Int]
  | A1ValConstTensorSubUpdate [Int]
  | A1ValConstTensorArgmax [Int] Int
  | A1ValConstTensorCrossEntropyForLogits Int Int
  | A1ValConstTensorCountEqual [Int]
  | A1ValConstBuiltInName Ass1BuiltInName -- TODO: consider merging `Ass1BuiltInName` and `Ass1ValConst`
  deriving stock (Eq, Show)

data Ass0TypeVal
  = A0TyValPrim Ass0PrimTypeVal (Maybe Ass0Val) -- Possibly equipped with a refinement predicate.
  | A0TyValList Ass0TypeVal (Maybe Ass0Val) -- Possibly equipped with a refinement predicate.
  | A0TyValArrow (Maybe AssVar, Ass0TypeVal) StrictAss0TypeExpr
  | A0TyValCode Ass1TypeVal
  deriving stock (Eq, Show)

data Ass0PrimTypeVal
  = A0TyValInt
  | A0TyValFloat
  | A0TyValBool
  | A0TyValUnit
  | A0TyValTensor [Int]
  deriving stock (Eq, Show)

data Ass1TypeVal
  = A1TyValPrim Ass1PrimTypeVal
  | A1TyValList Ass1TypeVal
  | A1TyValArrow Ass1TypeVal Ass1TypeVal
  deriving stock (Eq, Show)

data Ass1PrimTypeVal
  = A1TyValInt
  | A1TyValFloat
  | A1TyValBool
  | A1TyValUnit
  | A1TyValTensor [Int]
  deriving stock (Eq, Show)

data Type1Equation
  = TyEq1Prim Type1PrimEquation
  | TyEq1List Type1Equation
  | TyEq1Arrow Type1Equation Type1Equation
  deriving stock (Eq, Show)

data Type1PrimEquation
  = TyEq1Int
  | TyEq1Float
  | TyEq1Bool
  | TyEq1Unit
  | TyEq1TensorByLiteral [(Ass0Expr, Ass0Expr)]
  | TyEq1TensorByWhole Ass0Expr Ass0Expr -- A Pair of ASTs of type `List Nat`
  deriving stock (Eq, Show)

type EvalEnv = Map AssVar EvalEnvEntry

data EvalEnvEntry
  = Ass0ValEntry Ass0Val
  | SymbolEntry Symbol
  deriving stock (Eq, Show)

mapAssLiteral :: (e1 -> e2) -> AssLiteral e1 -> AssLiteral e2
mapAssLiteral f = \case
  ALitInt n -> ALitInt n
  ALitFloat r -> ALitFloat r
  ALitBool b -> ALitBool b
  ALitUnit -> ALitUnit
  ALitList es -> ALitList (map f es)
  ALitVec vec -> ALitVec vec
  ALitMat mat -> ALitMat mat

mapMAssLiteral :: (Monad m) => (e -> m v) -> AssLiteral e -> m (AssLiteral v)
mapMAssLiteral eval = \case
  ALitInt n -> pure $ ALitInt n
  ALitFloat r -> pure $ ALitFloat r
  ALitBool b -> pure $ ALitBool b
  ALitUnit -> pure ALitUnit
  ALitList a0es -> ALitList <$> mapM eval a0es
  ALitVec vec -> pure $ ALitVec vec
  ALitMat mat -> pure $ ALitMat mat

strictify :: Ass0TypeExpr -> StrictAss0TypeExpr
strictify = \case
  A0TyPrim a0tyPrim maybePred -> SA0TyPrim a0tyPrim maybePred
  A0TyList a0tye maybePred -> SA0TyList (strictify a0tye) maybePred
  A0TyArrow (x1opt, a0tye1) a0tye2 -> SA0TyArrow (x1opt, strictify a0tye1) (strictify a0tye2)
  A0TyCode a1tye1 -> SA0TyCode a1tye1
  A0TyOptArrow (x1, a0tye1) a0tye2 -> SA0TyArrow (Just x1, strictify a0tye1) (strictify a0tye2)

a0TyVec :: Int -> Ass0PrimType
a0TyVec n = A0TyTensor [n]

a0TyMat :: Int -> Int -> Ass0PrimType
a0TyMat m n = A0TyTensor [m, n]

a1TyVec :: Ass0Expr -> Ass1PrimType
a1TyVec a0e = A1TyTensor (A0Literal (ALitList [a0e]))

a1TyMat :: Ass0Expr -> Ass0Expr -> Ass1PrimType
a1TyMat a0e1 a0e2 = A1TyTensor (A0Literal (ALitList [a0e1, a0e2]))

decomposeType1Equation :: Type1Equation -> (Ass1TypeExpr, Ass1TypeExpr)
decomposeType1Equation = \case
  TyEq1Prim ty1eqPrim ->
    case ty1eqPrim of
      TyEq1Int -> prims A1TyInt
      TyEq1Float -> prims A1TyFloat
      TyEq1Bool -> prims A1TyBool
      TyEq1Unit -> prims A1TyUnit
      TyEq1TensorByLiteral zipped ->
        let a0eList1 = A0Literal (ALitList (map fst zipped))
            a0eList2 = A0Literal (ALitList (map snd zipped))
         in (A1TyPrim (A1TyTensor a0eList1), A1TyPrim (A1TyTensor a0eList2))
      TyEq1TensorByWhole a0eList1 a0eList2 ->
        (A1TyPrim (A1TyTensor a0eList1), A1TyPrim (A1TyTensor a0eList2))
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
