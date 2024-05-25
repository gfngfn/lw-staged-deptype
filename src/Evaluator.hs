module Evaluator
  ( evalExpr0,
    evalExpr1,
    initialState,
    unliftVal,
    Bug,
    EvalError,
    EvalState,
  )
where

import BuiltIn qualified
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Map qualified as Map
import Syntax

data Bug
  = UnboundVar Var
  | NotAClosure Ass0Val
  | NotACodeValue Ass0Val
  | NotAnInteger (Maybe Var) Ass0Val
  | FoundSymbol Var Symbol
  | FoundAss0Val Var Ass0Val
  deriving stock (Eq, Show)

data EvalError
  = Bug Bug
  | AssertionFailure Ass0Val Ass0Val
  deriving stock (Eq, Show)

data EvalState = EvalState
  { nextSymbolIndex :: Int
  }

type M a = StateT EvalState (Either EvalError) a

evalError :: EvalError -> M a
evalError = lift . Left

bug :: Bug -> M a
bug = lift . Left . Bug

initialState :: EvalState
initialState = EvalState {nextSymbolIndex = 0}

generateFreshSymbol :: M Symbol
generateFreshSymbol = do
  currentState@EvalState {nextSymbolIndex} <- get
  put $ currentState {nextSymbolIndex = nextSymbolIndex + 1}
  pure $ Symbol nextSymbolIndex

findEntry :: Env0 -> Var -> M EnvEntry
findEntry env x =
  case Map.lookup x env of
    Nothing -> bug $ UnboundVar x
    Just envEntry -> pure envEntry

findVal0 :: Env0 -> Var -> M Ass0Val
findVal0 env x = do
  entry <- findEntry env x
  case entry of
    Ass0ValEntry a0v -> pure a0v
    SymbolEntry symb -> bug $ FoundSymbol x symb

findSymbol :: Env0 -> Var -> M Symbol
findSymbol env x = do
  entry <- findEntry env x
  case entry of
    Ass0ValEntry a0v -> bug $ FoundAss0Val x a0v
    SymbolEntry symb -> pure symb

findInt0 :: Env0 -> Var -> M Int
findInt0 env x = do
  a0v <- findVal0 env x
  case a0v of
    A0ValLiteral (LitInt n) -> pure n
    _ -> bug $ NotAnInteger (Just x) a0v

evalExpr0 :: Env0 -> Ass0Expr -> M Ass0Val
evalExpr0 env = \case
  A0Literal lit ->
    pure $ A0ValLiteral lit
  A0AppBuiltIn bi ->
    case bi of
      BIAdd x1 x2 -> do
        n1 <- findInt0 env x1
        n2 <- findInt0 env x2
        pure $ A0ValLiteral (LitInt (n1 + n2))
      BIGenVadd x1 -> do
        n1 <- findInt0 env x1
        pure $ A0ValBracket (A1ValConst (A1ValConstVadd n1))
      BIGenVconcat x1 x2 -> do
        n1 <- findInt0 env x1
        n2 <- findInt0 env x2
        pure $ A0ValBracket (A1ValConst (A1ValConstVconcat n1 n2))
  A0Var x ->
    findVal0 env x
  A0Lam (x, a0tye1) a0e2 -> do
    a0tyv1 <- evalTypeExpr0 env a0tye1
    pure $ A0ValLam (x, a0tyv1) a0e2 env
  A0App a0e1 a0e2 -> do
    a0v1 <- evalExpr0 env a0e1
    a0v2 <- evalExpr0 env a0e2
    case a0v1 of
      A0ValLam (x, _a0tyv11) a0e12 env1 ->
        evalExpr0 (Map.insert x (Ass0ValEntry a0v2) env1) a0e12
      _ ->
        bug $ NotAClosure a0v1
  A0Bracket a1e1 -> do
    a1v1 <- evalExpr1 env a1e1
    pure $ A0ValBracket a1v1
  A0AssertAndThen a0e1 a0e2 a0e0 -> do
    a0v1 <- evalExpr0 env a0e1
    a0v2 <- evalExpr0 env a0e2
    if a0v1 == a0v2 -- Judges syntactic equality
      then evalExpr0 env a0e0
      else evalError $ AssertionFailure a0v1 a0v2

evalExpr1 :: Env0 -> Ass1Expr -> M Ass1Val
evalExpr1 env = \case
  A1Literal lit ->
    pure $ A1ValLiteral lit
  A1Var x -> do
    symb <- findSymbol env x
    pure $ A1ValVar symb
  A1Lam (x, a1tye1) a1e2 -> do
    a1tyv1 <- evalTypeExpr1 env a1tye1
    symb <- generateFreshSymbol
    a1v1 <- evalExpr1 (Map.insert x (SymbolEntry symb) env) a1e2
    pure $ A1ValLam (symb, a1tyv1) a1v1
  A1App a1e1 a1e2 -> do
    a1v1 <- evalExpr1 env a1e1
    a1v2 <- evalExpr1 env a1e2
    pure $ A1ValApp a1v1 a1v2
  A1Escape a0e1 -> do
    a0v1 <- evalExpr0 env a0e1
    case a0v1 of
      A0ValBracket a1v1 -> pure a1v1
      _ -> bug $ NotACodeValue a0v1

evalTypeExpr0 :: Env0 -> Ass0TypeExpr -> M Ass0TypeVal
evalTypeExpr0 env = \case
  A0TyPrim a0tyPrim ->
    pure . A0TyValPrim $
      case a0tyPrim of
        A0TyInt -> A0TyValInt
        A0TyBool -> A0TyValBool
        A0TyVec n -> A0TyValVec n
  A0TyArrow (xOpt, a0tye1) a0tye2 -> do
    a0tyv1 <- evalTypeExpr0 env a0tye1
    pure $ A0TyValArrow (xOpt, a0tyv1) a0tye2
  A0TyCode a1tye1 -> do
    a1tyv1 <- evalTypeExpr1 env a1tye1
    pure $ A0TyValCode a1tyv1

evalTypeExpr1 :: Env0 -> Ass1TypeExpr -> M Ass1TypeVal
evalTypeExpr1 env = \case
  A1TyPrim a1tyPrim ->
    A1TyValPrim
      <$> case a1tyPrim of
        A1TyInt -> pure A1TyValInt
        A1TyBool -> pure A1TyValBool
        A1TyVec a0e1 -> do
          a0v <- evalExpr0 env a0e1
          case a0v of
            A0ValLiteral (LitInt n) -> pure $ A1TyValVec n
            _ -> bug $ NotAnInteger Nothing a0v
  A1TyArrow a1tye1 a1tye2 -> do
    a1tyv1 <- evalTypeExpr1 env a1tye1
    a1tyv2 <- evalTypeExpr1 env a1tye2
    pure $ A1TyValArrow a1tyv1 a1tyv2

unliftVal :: Ass1Val -> Ass0Expr
unliftVal = \case
  A1ValLiteral lit -> A0Literal lit
  A1ValConst c ->
    case c of
      A1ValConstVadd n -> BuiltIn.ass0exprVadd n
      A1ValConstVconcat m n -> BuiltIn.ass0exprVconcat m n
  A1ValVar symb -> A0Var (symbolToVar symb)
  A1ValLam (symb, a1tyv1) a1v2 -> A0Lam (symbolToVar symb, unliftTypeVal a1tyv1) (unliftVal a1v2)
  A1ValApp a1v1 a1v2 -> A0App (unliftVal a1v1) (unliftVal a1v2)

unliftTypeVal :: Ass1TypeVal -> Ass0TypeExpr
unliftTypeVal = \case
  A1TyValPrim a1tyvPrim ->
    A0TyPrim $
      case a1tyvPrim of
        A1TyValInt -> A0TyInt
        A1TyValBool -> A0TyBool
        A1TyValVec n -> A0TyVec n
  A1TyValArrow a1tyv1 a1tyv2 ->
    A0TyArrow (Nothing, unliftTypeVal a1tyv1) (unliftTypeVal a1tyv2)
