module Lwsd.Evaluator
  ( evalExpr0,
    evalExpr1,
    initialState,
    unliftVal,
    Bug (..),
    EvalError (..),
    EvalState,
  )
where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Map qualified as Map
import Lwsd.BuiltIn qualified as BuiltIn
import Lwsd.Syntax
import Util.LocationInFile (SourceSpec, SpanInFile, getSpanInFile)
import Util.Matrix (Matrix)
import Util.Matrix qualified as Matrix
import Util.Vector (Vector)
import Util.Vector qualified as Vector
import Prelude

data Bug
  = UnboundVar AssVar
  | NotAClosure Ass0Val
  | NotACodeValue Ass0Val
  | NotAnInteger (Maybe AssVar) Ass0Val
  | NotAList (Maybe AssVar) Ass0Val
  | NotAVector AssVar Ass0Val
  | NotAMatrix AssVar Ass0Val
  | NotABoolean Ass0Val
  | NotAUnit Ass0Val
  | FoundSymbol AssVar Symbol
  | FoundAss0Val AssVar Ass0Val
  | InconsistentAppBuiltIn BuiltIn
  deriving stock (Eq, Show)

data EvalError
  = Bug Bug
  | AssertionFailure SpanInFile Ass1TypeVal Ass1TypeVal
  | NatAssertionFailure SpanInFile Int

data EvalState = EvalState
  { nextSymbolIndex :: Int,
    sourceSpec :: SourceSpec -- For assertion failure
  }

type M a = StateT EvalState (Either EvalError) a

evalError :: EvalError -> M a
evalError = lift . Left

bug :: Bug -> M a
bug = lift . Left . Bug

initialState :: SourceSpec -> EvalState
initialState sourceSpec =
  EvalState {nextSymbolIndex = 0, sourceSpec}

generateFreshSymbol :: M Symbol
generateFreshSymbol = do
  currentState@EvalState {nextSymbolIndex} <- get
  put $ currentState {nextSymbolIndex = nextSymbolIndex + 1}
  pure $ Symbol nextSymbolIndex

generateIdentityFunction :: EvalEnv -> Ass0TypeVal -> M Ass0Val
generateIdentityFunction env a0tyv = do
  x <- symbolToVar <$> generateFreshSymbol
  pure $ A0ValLam Nothing (x, a0tyv) (A0Var x) env

findEntry :: EvalEnv -> AssVar -> M EvalEnvEntry
findEntry env x =
  case Map.lookup x env of
    Nothing -> bug $ UnboundVar x
    Just envEntry -> pure envEntry

findVal0 :: EvalEnv -> AssVar -> M Ass0Val
findVal0 env x = do
  entry <- findEntry env x
  case entry of
    Ass0ValEntry a0v -> pure a0v
    SymbolEntry symb -> bug $ FoundSymbol x symb

findSymbol :: EvalEnv -> AssVar -> M Symbol
findSymbol env x = do
  entry <- findEntry env x
  case entry of
    Ass0ValEntry a0v -> bug $ FoundAss0Val x a0v
    SymbolEntry symb -> pure symb

findInt0 :: EvalEnv -> AssVar -> M Int
findInt0 env x = do
  a0v <- findVal0 env x
  case a0v of
    A0ValLiteral (ALitInt n) -> pure n
    _ -> bug $ NotAnInteger (Just x) a0v

findList0 :: EvalEnv -> AssVar -> M [Ass0Val]
findList0 env x = do
  a0v <- findVal0 env x
  case a0v of
    A0ValLiteral (ALitList a0es) -> pure a0es
    _ -> bug $ NotAList (Just x) a0v

findVec0 :: EvalEnv -> AssVar -> M Vector
findVec0 env x = do
  a0v <- findVal0 env x
  case a0v of
    A0ValLiteral (ALitVec v) -> pure v
    _ -> bug $ NotAVector x a0v

findMat0 :: EvalEnv -> AssVar -> M Matrix
findMat0 env x = do
  a0v <- findVal0 env x
  case a0v of
    A0ValLiteral (ALitMat mat) -> pure mat
    _ -> bug $ NotAMatrix x a0v

reduceBeta :: Ass0Val -> Ass0Val -> M Ass0Val
reduceBeta a0v1 a0v2 =
  case a0v1 of
    A0ValLam Nothing (x, _a0tyv11) a0e12 env1 ->
      evalExpr0
        (Map.insert x (Ass0ValEntry a0v2) env1)
        a0e12
    A0ValLam (Just (f, _a0tyvRec)) (x, _a0tyv11) a0e12 env1 ->
      evalExpr0
        (Map.insert x (Ass0ValEntry a0v2) (Map.insert f (Ass0ValEntry a0v1) env1))
        a0e12
    _ ->
      bug $ NotAClosure a0v1

-- The implementation of the built-in function `drop_at`.
dropAt :: Int -> [a] -> [a]
dropAt _ [] = []
dropAt n (v : vs) = if n <= 0 then vs else v : dropAt (n - 1) vs

evalExpr0 :: EvalEnv -> Ass0Expr -> M Ass0Val
evalExpr0 env = \case
  A0Literal lit ->
    A0ValLiteral <$> mapMAssLiteral (evalExpr0 env) lit
  A0AppBuiltIn bi ->
    case bi of
      BIAdd x1 x2 -> do
        n1 <- findInt0 env x1
        n2 <- findInt0 env x2
        pure $ A0ValLiteral (ALitInt (n1 + n2))
      BISub x1 x2 -> do
        n1 <- findInt0 env x1
        n2 <- findInt0 env x2
        pure $ A0ValLiteral (ALitInt (n1 - n2))
      BIMult x1 x2 -> do
        n1 <- findInt0 env x1
        n2 <- findInt0 env x2
        pure $ A0ValLiteral (ALitInt (n1 * n2))
      BILeq x1 x2 -> do
        n1 <- findInt0 env x1
        n2 <- findInt0 env x2
        pure $ A0ValLiteral (ALitBool (n1 <= n2))
      BIAssertNat loc x1 -> do
        n1 <- findInt0 env x1
        if n1 >= 0
          then
            pure $ A0ValLiteral (ALitInt n1)
          else do
            EvalState {sourceSpec} <- get
            let spanInFile = getSpanInFile sourceSpec loc
            evalError $ NatAssertionFailure spanInFile n1
      BIListMap f x -> do
        a0vF <- findVal0 env f
        a0vsIn <- findList0 env x
        a0vsOut <- mapM (reduceBeta a0vF) a0vsIn
        pure $ A0ValLiteral (ALitList a0vsOut)
      BIGenVadd x1 -> do
        n1 <- findInt0 env x1
        pure $ A0ValBracket (A1ValConst (A1ValConstVadd n1))
      BIGenVconcat x1 x2 -> do
        n1 <- findInt0 env x1
        n2 <- findInt0 env x2
        pure $ A0ValBracket (A1ValConst (A1ValConstVconcat n1 n2))
      BIGenMtranspose x1 x2 -> do
        n1 <- findInt0 env x1
        n2 <- findInt0 env x2
        pure $ A0ValBracket (A1ValConst (A1ValConstMtranspose n1 n2))
      BIGenMmult x1 x2 x3 -> do
        n1 <- findInt0 env x1
        n2 <- findInt0 env x2
        n3 <- findInt0 env x3
        pure $ A0ValBracket (A1ValConst (A1ValConstMmult n1 n2 n3))
      BIGenMconcatVert x1 x2 x3 -> do
        n1 <- findInt0 env x1
        n2 <- findInt0 env x2
        n3 <- findInt0 env x3
        pure $ A0ValBracket (A1ValConst (A1ValConstMconcatVert n1 n2 n3))
      BITensorGenAdd x1 -> do
        a0vs <- findList0 env x1
        ns <- mapM validateIntLiteral a0vs
        pure $ A0ValBracket (A1ValConst (A1ValConstTensorAdd ns))
      BIVadd n x1 x2 -> do
        v1 <- findVec0 env x1
        v2 <- findVec0 env x2
        case Vector.add n v1 v2 of
          Just v -> pure $ A0ValLiteral (ALitVec v)
          Nothing -> bug $ InconsistentAppBuiltIn bi
      BIVconcat m n x1 x2 -> do
        v1 <- findVec0 env x1
        v2 <- findVec0 env x2
        case Vector.concat m n v1 v2 of
          Just v -> pure $ A0ValLiteral (ALitVec v)
          Nothing -> bug $ InconsistentAppBuiltIn bi
      BIMtranspose m n x1 -> do
        mat1 <- findMat0 env x1
        case Matrix.transpose m n mat1 of
          Just mat -> pure $ A0ValLiteral (ALitMat mat)
          Nothing -> bug $ InconsistentAppBuiltIn bi
      BIMmult k m n x1 x2 -> do
        mat1 <- findMat0 env x1
        mat2 <- findMat0 env x2
        case Matrix.mult k m n mat1 mat2 of
          Just mat -> pure $ A0ValLiteral (ALitMat mat)
          Nothing -> bug $ InconsistentAppBuiltIn bi
      BIMconcatVert m1 m2 n x1 x2 -> do
        mat1 <- findMat0 env x1
        mat2 <- findMat0 env x2
        case Matrix.concatVert m1 m2 n mat1 mat2 of
          Just mat -> pure $ A0ValLiteral (ALitMat mat)
          Nothing -> bug $ InconsistentAppBuiltIn bi
      BIDropAt x1 x2 -> do
        n1 <- findInt0 env x1
        a0vs2 <- findList0 env x2
        pure $ A0ValLiteral (ALitList (dropAt n1 a0vs2))
      BIListAppend x1 x2 -> do
        a0vs1 <- findList0 env x1
        a0vs2 <- findList0 env x2
        pure $ A0ValLiteral (ALitList (a0vs1 ++ a0vs2))
      BIListIter f x -> do
        a0vF <- findVal0 env f
        a0vsIn <- findList0 env x
        forM_ a0vsIn (reduceBeta a0vF >=> validateUnitLiteral)
        pure $ A0ValLiteral ALitUnit
      BIGenBroadcasted x1 x2 -> do
        a0vs1 <- findList0 env x1
        ns1 <- mapM validateIntLiteral a0vs1
        a0vs2 <- findList0 env x2
        ns2 <- mapM validateIntLiteral a0vs2
        pure $ A0ValBracket (A1ValConst (A1ValConstBroadcasted ns1 ns2))
      BITensorGenZeros x1 -> do
        a0vs1 <- findList0 env x1
        ns1 <- mapM validateIntLiteral a0vs1
        pure $ A0ValBracket (A1ValConst (A1ValConstTensorZeros ns1))
      BITensorGenMult x1 -> do
        a0vs1 <- findList0 env x1
        ns1 <- mapM validateIntLiteral a0vs1
        pure $ A0ValBracket (A1ValConst (A1ValConstTensorMult ns1))
      BITensorGenGrad x1 -> do
        a0vs1 <- findList0 env x1
        ns1 <- mapM validateIntLiteral a0vs1
        pure $ A0ValBracket (A1ValConst (A1ValConstTensorGrad ns1))
      BITensorGenZeroGrad x1 -> do
        a0vs1 <- findList0 env x1
        ns1 <- mapM validateIntLiteral a0vs1
        pure $ A0ValBracket (A1ValConst (A1ValConstTensorZeroGrad ns1))
      BITensorGenSubUpdate x1 -> do
        a0vs1 <- findList0 env x1
        ns1 <- mapM validateIntLiteral a0vs1
        pure $ A0ValBracket (A1ValConst (A1ValConstTensorSubUpdate ns1))
      BITensorGenArgmax x1 x2 -> do
        a0vs1 <- findList0 env x1
        ns1 <- mapM validateIntLiteral a0vs1
        n2 <- findInt0 env x2
        pure $ A0ValBracket (A1ValConst (A1ValConstTensorArgmax ns1 n2))
      BITensorGenCrossEntropyForLogits x1 x2 -> do
        n1 <- findInt0 env x1
        n2 <- findInt0 env x2
        pure $ A0ValBracket (A1ValConst (A1ValConstTensorCrossEntropyForLogits n1 n2))
      BITensorGenCountEqual x1 -> do
        a0vs <- findList0 env x1
        ns <- mapM validateIntLiteral a0vs
        pure $ A0ValBracket (A1ValConst (A1ValConstTensorCountEqual ns))
      BITensorAdd ns x1 x2 ->
        case ns of
          [n] -> do
            v1 <- findVec0 env x1
            v2 <- findVec0 env x2
            case Vector.add n v1 v2 of
              Just v -> pure $ A0ValLiteral (ALitVec v)
              Nothing -> bug $ InconsistentAppBuiltIn bi
          [m, n] -> do
            mat1 <- findMat0 env x1
            mat2 <- findMat0 env x2
            case Matrix.add m n mat1 mat2 of
              Just mat -> pure $ A0ValLiteral (ALitMat mat)
              Nothing -> bug $ InconsistentAppBuiltIn bi
          _ ->
            error "TODO: evalExpr0, BITadd, dimension >= 3"
  A0Var x ->
    findVal0 env x
  A0BuiltInName a0builtInName ->
    pure $ BuiltIn.getAss0Val a0builtInName
  A0Lam Nothing (x, a0tye1) a0e2 -> do
    a0tyv1 <- evalTypeExpr0 env a0tye1
    pure $ A0ValLam Nothing (x, a0tyv1) a0e2 env
  A0Lam (Just (f, a0tyeRec)) (x, a0tye1) a0e2 -> do
    a0tyvRec <- evalTypeExpr0 env a0tyeRec
    a0tyv1 <- evalTypeExpr0 env a0tye1
    pure $ A0ValLam (Just (f, a0tyvRec)) (x, a0tyv1) a0e2 env
  A0App a0e1 a0e2 -> do
    a0v1 <- evalExpr0 env a0e1
    a0v2 <- evalExpr0 env a0e2
    reduceBeta a0v1 a0v2
  A0LetIn (x, a0tye1) a0e1 a0e2 ->
    evalExpr0 env (A0App (A0Lam Nothing (x, a0tye1) a0e2) a0e1)
  A0Sequential a0e1 a0e2 -> do
    a0v1 <- evalExpr0 env a0e1
    () <- validateUnitLiteral a0v1
    evalExpr0 env a0e2
  A0IfThenElse a0e0 a0e1 a0e2 -> do
    a0v0 <- evalExpr0 env a0e0
    case a0v0 of
      A0ValLiteral (ALitBool b) ->
        if b
          then evalExpr0 env a0e1
          else evalExpr0 env a0e2
      _ ->
        bug $ NotABoolean a0v0
  A0Bracket a1e1 -> do
    a1v1 <- evalExpr1 env a1e1
    pure $ A0ValBracket a1v1
  A0TyEqAssert loc ty1eq -> do
    let (a1tye1, a1tye2) = decomposeType1Equation ty1eq
    a1tyv1 <- evalTypeExpr1 env a1tye1
    a1tyv2 <- evalTypeExpr1 env a1tye2
    if a1tyv1 == a1tyv2 -- We can use `==` for stage-1 types
      then generateIdentityFunction env (A0TyValCode a1tyv1)
      else do
        EvalState {sourceSpec} <- get
        let spanInFile = getSpanInFile sourceSpec loc
        evalError $ AssertionFailure spanInFile a1tyv1 a1tyv2

evalExpr1 :: EvalEnv -> Ass1Expr -> M Ass1Val
evalExpr1 env = \case
  A1Literal lit ->
    A1ValLiteral <$> mapMAssLiteral (evalExpr1 env) lit
  A1Var x -> do
    symb <- findSymbol env x
    pure $ A1ValVar symb
  A1BuiltInName a1builtInName ->
    pure $ A1ValConst (A1ValConstBuiltInName a1builtInName)
  A1Lam Nothing (x, a1tye1) a1e2 -> do
    a1tyv1 <- evalTypeExpr1 env a1tye1
    symbX <- generateFreshSymbol
    a1v1 <- evalExpr1 (Map.insert x (SymbolEntry symbX) env) a1e2
    pure $ A1ValLam Nothing (symbX, a1tyv1) a1v1
  A1Lam (Just (f, a1tyeRec)) (x, a1tye1) a1e2 -> do
    a1tyvRec <- evalTypeExpr1 env a1tyeRec
    a1tyv1 <- evalTypeExpr1 env a1tye1
    symbF <- generateFreshSymbol
    symbX <- generateFreshSymbol
    a1v1 <- evalExpr1 (Map.insert x (SymbolEntry symbX) (Map.insert f (SymbolEntry symbF) env)) a1e2
    pure $ A1ValLam (Just (symbF, a1tyvRec)) (symbX, a1tyv1) a1v1
  A1App a1e1 a1e2 -> do
    a1v1 <- evalExpr1 env a1e1
    a1v2 <- evalExpr1 env a1e2
    pure $ A1ValApp a1v1 a1v2
  A1Sequential a1e1 a1e2 -> do
    a1v1 <- evalExpr1 env a1e1
    a1v2 <- evalExpr1 env a1e2
    pure $ A1ValSequential a1v1 a1v2
  A1IfThenElse a1e0 a1e1 a1e2 -> do
    a1v0 <- evalExpr1 env a1e0
    a1v1 <- evalExpr1 env a1e1
    a1v2 <- evalExpr1 env a1e2
    pure $ A1ValIfThenElse a1v0 a1v1 a1v2
  A1Escape a0e1 -> do
    a0v1 <- evalExpr0 env a0e1
    case a0v1 of
      A0ValBracket a1v1 -> pure a1v1
      _ -> bug $ NotACodeValue a0v1

evalTypeExpr0 :: EvalEnv -> StrictAss0TypeExpr -> M Ass0TypeVal
evalTypeExpr0 env = \case
  SA0TyPrim a0tyPrim ->
    pure . A0TyValPrim $
      case a0tyPrim of
        A0TyInt -> A0TyValInt
        A0TyNat -> A0TyValNat
        A0TyFloat -> A0TyValFloat
        A0TyBool -> A0TyValBool
        A0TyUnit -> A0TyValUnit
        A0TyTensor n -> A0TyValTensor n
  SA0TyList sa0tye1 -> do
    a0tyv1 <- evalTypeExpr0 env sa0tye1
    pure $ A0TyValList a0tyv1
  SA0TyArrow (xOpt, sa0tye1) sa0tye2 -> do
    a0tyv1 <- evalTypeExpr0 env sa0tye1
    pure $ A0TyValArrow (xOpt, a0tyv1) sa0tye2
  SA0TyCode a1tye1 -> do
    a1tyv1 <- evalTypeExpr1 env a1tye1
    pure $ A0TyValCode a1tyv1

validateIntLiteral :: Ass0Val -> M Int
validateIntLiteral = \case
  A0ValLiteral (ALitInt n) -> pure n
  a0v -> bug $ NotAnInteger Nothing a0v

validateUnitLiteral :: Ass0Val -> M ()
validateUnitLiteral = \case
  A0ValLiteral ALitUnit -> pure ()
  a0v -> bug $ NotAUnit a0v

validateListValue :: Ass0Val -> M [Ass0Val]
validateListValue = \case
  A0ValLiteral (ALitList a0vs) -> pure a0vs
  a0v -> bug $ NotAList Nothing a0v

evalTypeExpr1 :: EvalEnv -> Ass1TypeExpr -> M Ass1TypeVal
evalTypeExpr1 env = \case
  A1TyPrim a1tyPrim ->
    A1TyValPrim
      <$> case a1tyPrim of
        A1TyInt -> pure A1TyValInt
        A1TyFloat -> pure A1TyValFloat
        A1TyBool -> pure A1TyValBool
        A1TyUnit -> pure A1TyValUnit
        A1TyTensor a0eList -> do
          a0v <- evalExpr0 env a0eList
          a0vs <- validateListValue a0v
          ns <- mapM validateIntLiteral a0vs
          pure $ A1TyValTensor ns
  A1TyList a1tye -> do
    a1tyv <- evalTypeExpr1 env a1tye
    pure $ A1TyValList a1tyv
  A1TyArrow a1tye1 a1tye2 -> do
    a1tyv1 <- evalTypeExpr1 env a1tye1
    a1tyv2 <- evalTypeExpr1 env a1tye2
    pure $ A1TyValArrow a1tyv1 a1tyv2

unliftVal :: Ass1Val -> Ass0Expr
unliftVal = \case
  A1ValLiteral lit ->
    A0Literal (mapAssLiteral unliftVal lit)
  A1ValConst c ->
    case c of
      A1ValConstVadd n -> BuiltIn.ass0exprVadd n
      A1ValConstVconcat m n -> BuiltIn.ass0exprVconcat m n
      A1ValConstMtranspose m n -> BuiltIn.ass0exprMtranspose m n
      A1ValConstMmult k m n -> BuiltIn.ass0exprMmult k m n
      A1ValConstMconcatVert m1 m2 n -> BuiltIn.ass0exprMconcatVert m1 m2 n
      A1ValConstTensorAdd ns -> BuiltIn.ass0exprTensorAdd ns
      A1ValConstBuiltInName a1builtInName -> A0BuiltInName (unliftBuiltInName a1builtInName)
      _ -> error $ "TODO: unliftVal, " ++ show c
  A1ValVar symbX ->
    A0Var (symbolToVar symbX)
  A1ValLam Nothing (symbX, a1tyv1) a1v2 ->
    A0Lam Nothing (symbolToVar symbX, unliftTypeVal a1tyv1) (unliftVal a1v2)
  A1ValLam (Just (symbF, a1tyvRec)) (symbX, a1tyv1) a1v2 ->
    A0Lam (Just (symbolToVar symbF, unliftTypeVal a1tyvRec)) (symbolToVar symbX, unliftTypeVal a1tyv1) (unliftVal a1v2)
  A1ValApp a1v1 a1v2 ->
    A0App (unliftVal a1v1) (unliftVal a1v2)
  A1ValSequential a1v1 a1v2 ->
    A0Sequential (unliftVal a1v1) (unliftVal a1v2)
  A1ValIfThenElse a1v0 a1v1 a1v2 ->
    A0IfThenElse (unliftVal a1v0) (unliftVal a1v1) (unliftVal a1v2)

unliftTypeVal :: Ass1TypeVal -> StrictAss0TypeExpr
unliftTypeVal = \case
  A1TyValPrim a1tyvPrim ->
    SA0TyPrim $
      case a1tyvPrim of
        A1TyValInt -> A0TyInt
        A1TyValFloat -> A0TyFloat
        A1TyValBool -> A0TyBool
        A1TyValUnit -> A0TyUnit
        A1TyValTensor ns -> A0TyTensor ns
  A1TyValList a1tyv ->
    SA0TyList (unliftTypeVal a1tyv)
  A1TyValArrow a1tyv1 a1tyv2 ->
    SA0TyArrow (Nothing, unliftTypeVal a1tyv1) (unliftTypeVal a1tyv2)
