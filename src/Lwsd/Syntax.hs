module Lwsd.Syntax
  ( AssVar (..),
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
    AppContextEntry (..),
    Result (..),
  )
where

import Data.Map (Map)
import Data.Text (Text)
import Data.Text qualified as Text
import Lwsd.BuiltIn.Core
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

data AssLiteral e
  = ALitInt Int
  | ALitFloat Double
  | ALitBool Bool
  | ALitUnit
  | ALitList [e]
  | ALitVec Vector
  | ALitMat Matrix
  deriving stock (Eq, Show)

data Ass0ExprF var
  = A0Literal (AssLiteral (Ass0ExprF var))
  | A0BuiltInName BuiltIn
  | A0Var var
  | A0Lam (Maybe (var, StrictAss0TypeExprF var)) (var, StrictAss0TypeExprF var) (Ass0ExprF var)
  | A0App (Ass0ExprF var) (Ass0ExprF var)
  | A0LetIn (var, StrictAss0TypeExprF var) (Ass0ExprF var) (Ass0ExprF var)
  | A0Sequential (Ass0ExprF var) (Ass0ExprF var)
  | A0IfThenElse (Ass0ExprF var) (Ass0ExprF var) (Ass0ExprF var)
  | A0Bracket (Ass1ExprF var)
  | A0TyEqAssert Span (Type1EquationF var)
  | A0RefinementAssert Span (Ass0ExprF var) (Ass0ExprF var) -- A predicate and a target.
  deriving stock (Eq, Show)

data Ass1ExprF var
  = A1Literal (AssLiteral (Ass1ExprF var))
  | A1Var var
  | A1BuiltInName Ass1BuiltIn
  | A1Lam (Maybe (var, Ass1TypeExprF var)) (var, Ass1TypeExprF var) (Ass1ExprF var)
  | A1App (Ass1ExprF var) (Ass1ExprF var)
  | A1Sequential (Ass1ExprF var) (Ass1ExprF var)
  | A1IfThenElse (Ass1ExprF var) (Ass1ExprF var) (Ass1ExprF var)
  | A1Escape (Ass0ExprF var)
  deriving stock (Eq, Show)

a1LetIn :: (var, Ass1TypeExprF var) -> Ass1ExprF var -> Ass1ExprF var -> Ass1ExprF var
a1LetIn (x, a1tye) a1e1 a1e2 =
  A1App (A1Lam Nothing (x, a1tye) a1e2) a1e1

data AssBindF var
  = ABind0 (var, StrictAss0TypeExprF var) (Ass0ExprF var)
  | ABind1 (var, Ass1TypeExprF var) (Ass1ExprF var)

makeExprFromBinds :: forall var. [AssBindF var] -> Ass0ExprF var -> Ass0ExprF var
makeExprFromBinds abinds' a0eFinal = go0 abinds'
  where
    go0 :: [AssBindF var] -> Ass0ExprF var
    go0 = \case
      [] -> a0eFinal
      ABind0 xsty a0e : abinds -> A0LetIn xsty a0e (go0 abinds)
      ABind1 xty a1e : abinds -> A0Bracket (a1LetIn xty a1e (go1 abinds))

    go1 :: [AssBindF var] -> Ass1ExprF var
    go1 = \case
      [] -> A1Escape a0eFinal
      ABind1 xty a1e : abinds -> a1LetIn xty a1e (go1 abinds)
      ABind0 xsty a0e : abinds -> A1Escape (A0LetIn xsty a0e (go0 abinds))

-- For type-checking.
data Ass0TypeExprF var
  = A0TyPrim Ass0PrimType (Maybe (Ass0ExprF var)) -- Possibly equipped with a refinement predicate.
  | A0TyList (Ass0TypeExprF var) (Maybe (Ass0ExprF var)) -- Possibly equipped with a refinement predicate.
  | A0TyArrow (Maybe var, Ass0TypeExprF var) (Ass0TypeExprF var)
  | A0TyOptArrow (var, Ass0TypeExprF var) (Ass0TypeExprF var)
  | A0TyCode (Ass1TypeExprF var)
  deriving stock (Eq, Show)

-- For type annotations in target terms.
data StrictAss0TypeExprF var
  = SA0TyPrim Ass0PrimType (Maybe (Ass0ExprF var)) -- Possibly equipped with a refinement predicate.
  | SA0TyList (StrictAss0TypeExprF var) (Maybe (Ass0ExprF var)) -- Possibly equipped with a refinement predicate.
  | SA0TyArrow (Maybe var, StrictAss0TypeExprF var) (StrictAss0TypeExprF var)
  | SA0TyCode (Ass1TypeExprF var)
  deriving stock (Eq, Show)

data Ass0PrimType
  = A0TyInt
  | A0TyFloat
  | A0TyBool
  | A0TyUnit
  | A0TyTensor [Int]
  deriving stock (Eq, Show)

data Ass1TypeExprF var
  = A1TyPrim (Ass1PrimTypeF var)
  | A1TyList (Ass1TypeExprF var)
  | A1TyArrow (Ass1TypeExprF var) (Ass1TypeExprF var)
  deriving stock (Eq, Show)

data Ass1PrimTypeF var
  = A1TyInt
  | A1TyFloat
  | A1TyBool
  | A1TyUnit
  | A1TyTensor (Ass0ExprF var)
  deriving stock (Eq, Show)

-- Types for persistent value items.
data AssPersTypeExpr
  = APersTyPrim Ass0PrimType
  | APersTyList AssPersTypeExpr
  | APersTyArrow AssPersTypeExpr AssPersTypeExpr
  deriving stock (Eq, Show)

persistentTypeTo0 :: AssPersTypeExpr -> Ass0TypeExprF var
persistentTypeTo0 = \case
  APersTyPrim a0tyPrim -> A0TyPrim a0tyPrim Nothing
  APersTyList aPtye -> A0TyList (persistentTypeTo0 aPtye) Nothing
  APersTyArrow aPtye1 aPtye2 -> A0TyArrow (Nothing, persistentTypeTo0 aPtye1) (persistentTypeTo0 aPtye2)

persistentTypeTo1 :: AssPersTypeExpr -> Ass1TypeExprF var
persistentTypeTo1 = \case
  APersTyPrim a0tyPrim -> A1TyPrim (liftPrimType a0tyPrim)
  APersTyList aPtye -> A1TyList (persistentTypeTo1 aPtye)
  APersTyArrow aPtye1 aPtye2 -> A1TyArrow (persistentTypeTo1 aPtye1) (persistentTypeTo1 aPtye2)

liftPrimType :: Ass0PrimType -> Ass1PrimTypeF var
liftPrimType = \case
  A0TyInt -> A1TyInt
  A0TyFloat -> A1TyFloat
  A0TyBool -> A1TyBool
  A0TyUnit -> A1TyUnit
  A0TyTensor ns -> A1TyTensor (A0Literal (ALitList (map (A0Literal . ALitInt) ns)))

data Ass0ValF var
  = A0ValLiteral (AssLiteral (Ass0ValF var))
  | A0ValLam (Maybe (var, Ass0TypeValF var)) (var, Ass0TypeValF var) (Ass0ExprF var) EvalEnv
  | A0ValBracket (Ass1ValF var)
  | A0ValPartialBuiltInApp (Ass0PartialBuiltInApp (Ass0ValF var))
  deriving stock (Eq, Show)

data Ass1ValF var
  = A1ValLiteral (AssLiteral (Ass1ValF var))
  | A1ValConst Ass1BuiltIn
  | A1ValVar Symbol
  | A1ValLam (Maybe (Symbol, Ass1TypeVal)) (Symbol, Ass1TypeVal) (Ass1ValF var)
  | A1ValApp (Ass1ValF var) (Ass1ValF var)
  | A1ValSequential (Ass1ValF var) (Ass1ValF var)
  | A1ValIfThenElse (Ass1ValF var) (Ass1ValF var) (Ass1ValF var)
  deriving stock (Eq, Show)

data Ass0TypeValF var
  = A0TyValPrim Ass0PrimTypeVal (Maybe (Ass0ValF var)) -- Possibly equipped with a refinement predicate.
  | A0TyValList (Ass0TypeValF var) (Maybe (Ass0ValF var)) -- Possibly equipped with a refinement predicate.
  | A0TyValArrow (Maybe var, Ass0TypeValF var) (StrictAss0TypeExprF var)
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

data Type1EquationF var
  = TyEq1Prim (Type1PrimEquationF var)
  | TyEq1List (Type1EquationF var)
  | TyEq1Arrow (Type1EquationF var) (Type1EquationF var)
  deriving stock (Eq, Show)

data Type1PrimEquationF var
  = TyEq1Int
  | TyEq1Float
  | TyEq1Bool
  | TyEq1Unit
  | TyEq1TensorByLiteral [(Ass0ExprF var, Ass0ExprF var)]
  | TyEq1TensorByWhole (Ass0ExprF var) (Ass0ExprF var) -- A Pair of ASTs of type `List Nat`
  deriving stock (Eq, Show)

type EvalEnv = Map AssVar EvalEnvEntry

data EvalEnvEntry
  = Ass0ValEntry (Ass0ValF AssVar)
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

strictify :: Ass0TypeExprF var -> StrictAss0TypeExprF var
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

decomposeType1Equation :: Type1EquationF var -> (Ass1TypeExprF var, Ass1TypeExprF var)
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

type Ass0Expr = Ass0ExprF AssVar

type Ass1Expr = Ass1ExprF AssVar

type AssBind = AssBindF AssVar

type Type1Equation = Type1EquationF AssVar

type Ass0TypeExpr = Ass0TypeExprF AssVar

type StrictAss0TypeExpr = StrictAss0TypeExprF AssVar

type Ass1TypeExpr = Ass1TypeExprF AssVar

type Ass1PrimType = Ass1PrimTypeF AssVar

type Ass0Val = Ass0ValF AssVar

type Ass1Val = Ass1ValF AssVar

type Ass0TypeVal = Ass0TypeValF AssVar

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
