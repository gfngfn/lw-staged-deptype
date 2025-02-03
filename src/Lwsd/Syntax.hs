module Lwsd.Syntax
  ( StaticVar (..),
    AssVarF (..),
    Symbol (..),
    symbolToVar,
    AssLiteral (..),
    Ass0ExprF (..),
    Ass1ExprF (..),
    a1LetIn,
    AssBindF (..),
    makeExprFromBinds,
    Type1EquationF (..),
    Type1PrimEquationF (..),
    Ass0TypeExprF (..),
    StrictAss0TypeExprF (..),
    Ass0PrimType (..),
    Ass1TypeExprF (..),
    Ass1PrimTypeF (..),
    AssPersTypeExpr (..),
    persistentTypeTo0,
    persistentTypeTo1,
    liftPrimType,
    Ass0ValF (..),
    Ass1ValF (..),
    Ass0TypeValF (..),
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
    AppContextF,
    AppContextEntryF (..),
    ResultF (..),
    AssVar,
    Ass0Expr,
    Ass1Expr,
    AssBind,
    Type1Equation,
    Ass0TypeExpr,
    StrictAss0TypeExpr,
    Ass1TypeExpr,
    Ass0Val,
    Ass1Val,
    Ass0TypeVal,
    Ass1PrimType,
    AppContext,
    Result,
  )
where

import Data.Map (Map)
import Lwsd.BuiltIn.Core
import Util.Matrix (Matrix)
import Util.TokenUtil (Span)
import Util.Vector (Vector)
import Prelude

newtype StaticVar = StaticVar Int
  deriving newtype (Eq, Ord, Show)

data AssVarF sv
  = AssVarStatic sv
  | AssVarDynamic Int
  deriving stock (Eq, Ord, Show, Functor)

-- The type for symbols generated dynamically for hygienicity.
newtype Symbol = Symbol Int
  deriving newtype (Eq, Show)

symbolToVar :: Symbol -> AssVarF sv
symbolToVar (Symbol n) = AssVarDynamic n

data AssLiteral e
  = ALitInt Int
  | ALitFloat Double
  | ALitBool Bool
  | ALitUnit
  | ALitList [e]
  | ALitVec Vector
  | ALitMat Matrix
  deriving stock (Eq, Show, Functor)

data Ass0ExprF sv
  = A0Literal (AssLiteral (Ass0ExprF sv))
  | A0BuiltInName BuiltIn
  | A0Var (AssVarF sv)
  | A0Lam (Maybe (AssVarF sv, StrictAss0TypeExprF sv)) (AssVarF sv, StrictAss0TypeExprF sv) (Ass0ExprF sv)
  | A0App (Ass0ExprF sv) (Ass0ExprF sv)
  | A0LetIn (AssVarF sv, StrictAss0TypeExprF sv) (Ass0ExprF sv) (Ass0ExprF sv)
  | A0Sequential (Ass0ExprF sv) (Ass0ExprF sv)
  | A0IfThenElse (Ass0ExprF sv) (Ass0ExprF sv) (Ass0ExprF sv)
  | A0Bracket (Ass1ExprF sv)
  | A0TyEqAssert Span (Type1EquationF sv)
  | A0RefinementAssert Span (Ass0ExprF sv) (Ass0ExprF sv) -- A predicate and a target.
  deriving stock (Eq, Show, Functor)

data Ass1ExprF sv
  = A1Literal (AssLiteral (Ass1ExprF sv))
  | A1Var (AssVarF sv)
  | A1BuiltInName Ass1BuiltIn
  | A1Lam (Maybe (AssVarF sv, Ass1TypeExprF sv)) (AssVarF sv, Ass1TypeExprF sv) (Ass1ExprF sv)
  | A1App (Ass1ExprF sv) (Ass1ExprF sv)
  | A1Sequential (Ass1ExprF sv) (Ass1ExprF sv)
  | A1IfThenElse (Ass1ExprF sv) (Ass1ExprF sv) (Ass1ExprF sv)
  | A1Escape (Ass0ExprF sv)
  deriving stock (Eq, Show, Functor)

a1LetIn :: (AssVarF sv, Ass1TypeExprF sv) -> Ass1ExprF sv -> Ass1ExprF sv -> Ass1ExprF sv
a1LetIn (x, a1tye) a1e1 a1e2 =
  A1App (A1Lam Nothing (x, a1tye) a1e2) a1e1

data AssBindF sv
  = ABind0 (AssVarF sv, StrictAss0TypeExprF sv) (Ass0ExprF sv)
  | ABind1 (AssVarF sv, Ass1TypeExprF sv) (Ass1ExprF sv)
  deriving stock (Eq, Show, Functor)

makeExprFromBinds :: forall sv. [AssBindF sv] -> Ass0ExprF sv -> Ass0ExprF sv
makeExprFromBinds abinds' a0eFinal = go0 abinds'
  where
    go0 :: [AssBindF sv] -> Ass0ExprF sv
    go0 = \case
      [] -> a0eFinal
      ABind0 xsty a0e : abinds -> A0LetIn xsty a0e (go0 abinds)
      ABind1 xty a1e : abinds -> A0Bracket (a1LetIn xty a1e (go1 abinds))

    go1 :: [AssBindF sv] -> Ass1ExprF sv
    go1 = \case
      [] -> A1Escape a0eFinal
      ABind1 xty a1e : abinds -> a1LetIn xty a1e (go1 abinds)
      ABind0 xsty a0e : abinds -> A1Escape (A0LetIn xsty a0e (go0 abinds))

-- For type-checking.
data Ass0TypeExprF sv
  = A0TyPrim Ass0PrimType (Maybe (Ass0ExprF sv)) -- Possibly equipped with a refinement predicate.
  | A0TyList (Ass0TypeExprF sv) (Maybe (Ass0ExprF sv)) -- Possibly equipped with a refinement predicate.
  | A0TyArrow (Maybe (AssVarF sv), Ass0TypeExprF sv) (Ass0TypeExprF sv)
  | A0TyOptArrow (AssVarF sv, Ass0TypeExprF sv) (Ass0TypeExprF sv)
  | A0TyCode (Ass1TypeExprF sv)
  deriving stock (Eq, Show, Functor)

-- For type annotations in target terms.
data StrictAss0TypeExprF sv
  = SA0TyPrim Ass0PrimType (Maybe (Ass0ExprF sv)) -- Possibly equipped with a refinement predicate.
  | SA0TyList (StrictAss0TypeExprF sv) (Maybe (Ass0ExprF sv)) -- Possibly equipped with a refinement predicate.
  | SA0TyArrow (Maybe (AssVarF sv), StrictAss0TypeExprF sv) (StrictAss0TypeExprF sv)
  | SA0TyCode (Ass1TypeExprF sv)
  deriving stock (Eq, Show, Functor)

data Ass0PrimType
  = A0TyInt
  | A0TyFloat
  | A0TyBool
  | A0TyUnit
  | A0TyTensor [Int]
  deriving stock (Eq, Show)

data Ass1TypeExprF sv
  = A1TyPrim (Ass1PrimTypeF sv)
  | A1TyList (Ass1TypeExprF sv)
  | A1TyArrow (Ass1TypeExprF sv) (Ass1TypeExprF sv)
  deriving stock (Eq, Show, Functor)

data Ass1PrimTypeF sv
  = A1TyInt
  | A1TyFloat
  | A1TyBool
  | A1TyUnit
  | A1TyTensor (Ass0ExprF sv)
  deriving stock (Eq, Show, Functor)

-- Types for persistent value items.
data AssPersTypeExpr
  = APersTyPrim Ass0PrimType
  | APersTyList AssPersTypeExpr
  | APersTyArrow AssPersTypeExpr AssPersTypeExpr
  deriving stock (Eq, Show)

persistentTypeTo0 :: AssPersTypeExpr -> Ass0TypeExprF sv
persistentTypeTo0 = \case
  APersTyPrim a0tyPrim -> A0TyPrim a0tyPrim Nothing
  APersTyList aPtye -> A0TyList (persistentTypeTo0 aPtye) Nothing
  APersTyArrow aPtye1 aPtye2 -> A0TyArrow (Nothing, persistentTypeTo0 aPtye1) (persistentTypeTo0 aPtye2)

persistentTypeTo1 :: AssPersTypeExpr -> Ass1TypeExprF sv
persistentTypeTo1 = \case
  APersTyPrim a0tyPrim -> A1TyPrim (liftPrimType a0tyPrim)
  APersTyList aPtye -> A1TyList (persistentTypeTo1 aPtye)
  APersTyArrow aPtye1 aPtye2 -> A1TyArrow (persistentTypeTo1 aPtye1) (persistentTypeTo1 aPtye2)

liftPrimType :: Ass0PrimType -> Ass1PrimTypeF sv
liftPrimType = \case
  A0TyInt -> A1TyInt
  A0TyFloat -> A1TyFloat
  A0TyBool -> A1TyBool
  A0TyUnit -> A1TyUnit
  A0TyTensor ns -> A1TyTensor (A0Literal (ALitList (map (A0Literal . ALitInt) ns)))

data Ass0ValF sv
  = A0ValLiteral (AssLiteral (Ass0ValF sv))
  | A0ValLam (Maybe (AssVarF sv, Ass0TypeValF sv)) (AssVarF sv, Ass0TypeValF sv) (Ass0ExprF sv) EvalEnv
  | A0ValBracket (Ass1ValF sv)
  | A0ValPartialBuiltInApp (Ass0PartialBuiltInApp (Ass0ValF sv))
  deriving stock (Eq, Show)

data Ass1ValF sv
  = A1ValLiteral (AssLiteral (Ass1ValF sv))
  | A1ValConst Ass1BuiltIn
  | A1ValVar Symbol
  | A1ValLam (Maybe (Symbol, Ass1TypeVal)) (Symbol, Ass1TypeVal) (Ass1ValF sv)
  | A1ValApp (Ass1ValF sv) (Ass1ValF sv)
  | A1ValSequential (Ass1ValF sv) (Ass1ValF sv)
  | A1ValIfThenElse (Ass1ValF sv) (Ass1ValF sv) (Ass1ValF sv)
  deriving stock (Eq, Show)

data Ass0TypeValF sv
  = A0TyValPrim Ass0PrimTypeVal (Maybe (Ass0ValF sv)) -- Possibly equipped with a refinement predicate.
  | A0TyValList (Ass0TypeValF sv) (Maybe (Ass0ValF sv)) -- Possibly equipped with a refinement predicate.
  | A0TyValArrow (Maybe (AssVarF sv), Ass0TypeValF sv) (StrictAss0TypeExprF sv)
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

data Type1EquationF sv
  = TyEq1Prim (Type1PrimEquationF sv)
  | TyEq1List (Type1EquationF sv)
  | TyEq1Arrow (Type1EquationF sv) (Type1EquationF sv)
  deriving stock (Eq, Show, Functor)

data Type1PrimEquationF sv
  = TyEq1Int
  | TyEq1Float
  | TyEq1Bool
  | TyEq1Unit
  | TyEq1TensorByLiteral [(Ass0ExprF sv, Ass0ExprF sv)]
  | TyEq1TensorByWhole (Ass0ExprF sv) (Ass0ExprF sv) -- A Pair of ASTs of type `List Nat`
  deriving stock (Eq, Show, Functor)

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

strictify :: Ass0TypeExprF sv -> StrictAss0TypeExprF sv
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

a1TyVec :: Ass0ExprF ann -> Ass1PrimTypeF ann
a1TyVec a0e = A1TyTensor (A0Literal (ALitList [a0e]))

a1TyMat :: Ass0ExprF ann -> Ass0ExprF ann -> Ass1PrimTypeF ann
a1TyMat a0e1 a0e2 = A1TyTensor (A0Literal (ALitList [a0e1, a0e2]))

decomposeType1Equation :: Type1EquationF sv -> (Ass1TypeExprF sv, Ass1TypeExprF sv)
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

type AppContextF sv = [AppContextEntryF sv]

data AppContextEntryF sv
  = AppArg0 (Ass0ExprF sv) (Ass0TypeExprF sv)
  | AppArg1 (Ass1TypeExprF sv)
  | AppArgOptGiven0 (Ass0ExprF sv) (Ass0TypeExprF sv)
  | AppArgOptOmitted0
  deriving (Eq, Show, Functor)

data ResultF a sv
  = Pure a
  | Cast0 (Maybe (Ass0ExprF sv)) (Ass0TypeExprF sv) (ResultF a sv)
  | Cast1 (Maybe (Ass0ExprF sv)) (Ass1TypeExprF sv) (ResultF a sv)
  | CastGiven0 (Maybe (Ass0ExprF sv)) (Ass0TypeExprF sv) (ResultF a sv)
  | FillInferred0 (Ass0ExprF sv) (ResultF a sv)
  | InsertInferred0 (Ass0ExprF sv) (ResultF a sv)
  deriving (Eq, Show, Functor)

type AssVar = AssVarF StaticVar

type Ass0Expr = Ass0ExprF StaticVar

type Ass1Expr = Ass1ExprF StaticVar

type AssBind = AssBindF StaticVar

type Type1Equation = Type1EquationF StaticVar

type Ass0TypeExpr = Ass0TypeExprF StaticVar

type StrictAss0TypeExpr = StrictAss0TypeExprF StaticVar

type Ass1TypeExpr = Ass1TypeExprF StaticVar

type Ass1PrimType = Ass1PrimTypeF StaticVar

type Ass0Val = Ass0ValF StaticVar

type Ass1Val = Ass1ValF StaticVar

type Ass0TypeVal = Ass0TypeValF StaticVar

type AppContext = AppContextF StaticVar

type Result a = ResultF a StaticVar
