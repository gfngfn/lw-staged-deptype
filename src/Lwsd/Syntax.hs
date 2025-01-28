module Lwsd.Syntax
  ( AssVar (..),
    Symbol (..),
    symbolToVar,
    BuiltIn (..),
    Ass0BuiltInName (..),
    Ass1BuiltInName (..),
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
  = BIAdd AssVar AssVar
  | BISub AssVar AssVar
  | BIMult AssVar AssVar
  | BILeq AssVar AssVar
  | BIAssertNat Span AssVar
  | BIListMap AssVar AssVar
  | BIGenVadd AssVar
  | BIGenVconcat AssVar AssVar
  | BIGenMtranspose AssVar AssVar
  | BIGenMmult AssVar AssVar AssVar
  | BIGenMconcatVert AssVar AssVar AssVar
  | BIGenTadd AssVar
  | BIVadd Int AssVar AssVar
  | BIVconcat Int Int AssVar AssVar
  | BIMtranspose Int Int AssVar
  | BIMmult Int Int Int AssVar AssVar
  | BIMconcatVert Int Int Int AssVar AssVar
  | BIDropAt AssVar AssVar
  | BITensorGenCountEqual AssVar
  | BITadd [Int] AssVar AssVar
  deriving stock (Eq, Show)

data Ass0BuiltInName
  = A0BINameAdd
  | A0BINameSub
  | A0BINameMult
  | A0BINameFloatDiv
  | A0BINameLeq
  | A0BINameFloat
  | A0BINameGenVadd
  | A0BINameGenVconcat
  | A0BINameGenMtranspose
  | A0BINameGenMmult
  | A0BINameGenMconcatVert
  | A0BINameDropAt
  | A0BINameTensorGenZeros
  | A0BINameTensorGenMult
  | A0BINameTensorGenGrad
  | A0BINameTensorGenZeroGrad
  | A0BINameTensorGenSubUpdate
  | A0BINameTensorGenArgmax
  | A0BINameTensorGenCrossEntropyForLogits
  | A0BINameTensorGenCountEqual
  | A0BINameGenTadd
  | A0BINamePrintFloat
  | A0BINameListAppend
  | A0BINameListIter
  | A0BINameRange
  | A0BINameGenBroadcasted
  | A0BINameTensorF
  | A0BINameTensorBackward
  | A0BINameTensorNoGrad
  | A0BINameTensorFloatValue
  | A0BINameMnistHelperTrainImages
  | A0BINameMnistHelperTrainLabels
  | A0BINameMnistHelperTestImages
  | A0BINameMnistHelperTestLabels
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

unliftBuiltInName :: Ass1BuiltInName -> Ass0BuiltInName
unliftBuiltInName = \case
  A1BINameAdd -> A0BINameAdd
  A1BINameSub -> A0BINameSub
  A1BINameMult -> A0BINameMult
  A1BINameFloatDiv -> A0BINameFloatDiv
  A1BINameLeq -> A0BINameLeq
  A1BINameFloat -> A0BINameFloat
  A1BINamePrintFloat -> A0BINamePrintFloat
  A1BINameListAppend -> A0BINameListAppend
  A1BINameListIter -> A0BINameListIter
  A1BINameRange -> A0BINameRange
  A1BINameTensorF -> A0BINameTensorF
  A1BINameTensorBackward -> A0BINameTensorBackward
  A1BINameTensorNoGrad -> A0BINameTensorNoGrad
  A1BINameTensorFloatValue -> A0BINameTensorFloatValue
  A1BINameMnistHelperTrainImages -> A0BINameMnistHelperTrainImages
  A1BINameMnistHelperTrainLabels -> A0BINameMnistHelperTrainLabels
  A1BINameMnistHelperTestImages -> A0BINameMnistHelperTestImages
  A1BINameMnistHelperTestLabels -> A0BINameMnistHelperTestLabels

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
  | A0AppBuiltIn BuiltIn
  | A0Var AssVar
  | A0BuiltInName Ass0BuiltInName
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
  | A1BuiltInName Ass1BuiltInName
  | A1Lam (Maybe (AssVar, Ass1TypeExpr)) (AssVar, Ass1TypeExpr) Ass1Expr
  | A1App Ass1Expr Ass1Expr
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
  APersTyPrim a0tyPrim -> A0TyPrim a0tyPrim
  APersTyList aPtye -> A0TyList (persistentTypeTo0 aPtye)
  APersTyArrow aPtye1 aPtye2 -> A0TyArrow (Nothing, persistentTypeTo0 aPtye1) (persistentTypeTo0 aPtye2)

persistentTypeTo1 :: AssPersTypeExpr -> Ass1TypeExpr
persistentTypeTo1 = \case
  APersTyPrim a0tyPrim -> A1TyPrim (liftPrimType a0tyPrim)
  APersTyList aPtye -> A1TyList (persistentTypeTo1 aPtye)
  APersTyArrow aPtye1 aPtye2 -> A1TyArrow (persistentTypeTo1 aPtye1) (persistentTypeTo1 aPtye2)

liftPrimType :: Ass0PrimType -> Ass1PrimType
liftPrimType = \case
  A0TyInt -> A1TyInt
  A0TyNat -> A1TyInt
  A0TyFloat -> A1TyFloat
  A0TyBool -> A1TyBool
  A0TyUnit -> A1TyUnit
  A0TyTensor ns -> A1TyTensor (A0Literal (ALitList (map (A0Literal . ALitInt) ns)))

data Ass0Val
  = A0ValLiteral (AssLiteral Ass0Val)
  | A0ValLam (Maybe (AssVar, Ass0TypeVal)) (AssVar, Ass0TypeVal) Ass0Expr EvalEnv
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
  | A1ValConstTensorCountEqual [Int]
  | A1ValConstTadd [Int]
  | A1ValConstBuiltInName Ass1BuiltInName -- TODO: consider merging `Ass1BuiltInName` and `Ass1ValConst`
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
