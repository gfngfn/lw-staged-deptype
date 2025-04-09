module Lwsd.Evaluator
  ( evalExpr0,
    evalExpr1,
    initialState,
    run,
    unliftVal,
    EvalState,
  )
where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Lwsd.BuiltIn.Core
import Lwsd.EvalError
import Lwsd.Syntax
import Util.LocationInFile (SourceSpec, getSpanInFile)
import Util.Matrix (Matrix)
import Util.Matrix qualified as Matrix
import Util.Vector (Vector)
import Util.Vector qualified as Vector
import Prelude

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
    Nothing -> bug $ UnboundVarFound x
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

validateIntLiteral :: Ass0Val -> M Int
validateIntLiteral = \case
  A0ValLiteral (ALitInt n) -> pure n
  a0v -> bug $ NotAnInteger a0v

validateBoolLiteral :: Ass0Val -> M Bool
validateBoolLiteral = \case
  A0ValLiteral (ALitBool b) -> pure b
  a0v -> bug $ NotABoolean a0v

validateUnitLiteral :: Ass0Val -> M ()
validateUnitLiteral = \case
  A0ValLiteral ALitUnit -> pure ()
  a0v -> bug $ NotAUnit a0v

validateListValue :: Ass0Val -> M [Ass0Val]
validateListValue = \case
  A0ValLiteral (ALitList a0vs) -> pure a0vs
  a0v -> bug $ NotAList a0v

validateIntListLiteral :: Ass0Val -> M [Int]
validateIntListLiteral a0v = do
  a0vs <- validateListValue a0v
  mapM validateIntLiteral a0vs

validateVec0 :: Ass0Val -> M Vector
validateVec0 = \case
  A0ValLiteral (ALitVec v) -> pure v
  a0v -> bug $ NotAVector a0v

validateMat0 :: Ass0Val -> M Matrix
validateMat0 = \case
  A0ValLiteral (ALitMat mat) -> pure mat
  a0v -> bug $ NotAMatrix a0v

-- The implementation of the built-in function `drop_at`.
dropAt :: Int -> [a] -> [a]
dropAt _ [] = []
dropAt n (v : vs) = if n <= 0 then vs else v : dropAt (n - 1) vs

-- The implementation of the built-in function `drop_at`.
broadcast :: [Int] -> [Int] -> Maybe [Int]
broadcast ns1' ns2' = reverse <$> go (reverse ns1', reverse ns2')
  where
    go = \case
      ([], ns2) -> pure ns2
      (ns1, []) -> pure ns1
      (n1 : ns1, 1 : ns2) -> (n1 :) <$> broadcast ns1 ns2
      (1 : ns1, n2 : ns2) -> (n2 :) <$> broadcast ns1 ns2
      (n1 : ns1, n2 : ns2) | n1 == n2 -> (n1 :) <$> broadcast ns1 ns2
      _ -> Nothing

reduceDeltaArity1 :: BuiltInArity1 -> Ass0Val -> M Ass0Val
reduceDeltaArity1 bi1 a0v1 =
  case bi1 of
    BIGenVadd -> do
      n1 <- validateIntLiteral a0v1
      pure $ A0ValBracket (A1ValConst (A1BIVadd n1))
    BIMtranspose m n -> do
      mat1 <- validateMat0 a0v1
      case Matrix.transpose m n mat1 of
        Just mat -> pure $ A0ValLiteral (ALitMat mat)
        Nothing -> bug $ InconsistentAppBuiltInArity1 bi1 a0v1
    BITensorGenZeros -> do
      ns1 <- validateIntListLiteral a0v1
      pure $ A0ValBracket (A1ValConst (A1BITensorZeros ns1))
    BITensorGenGrad -> do
      ns1 <- validateIntListLiteral a0v1
      pure $ A0ValBracket (A1ValConst (A1BITensorGrad ns1))
    BITensorGenZeroGrad -> do
      ns1 <- validateIntListLiteral a0v1
      pure $ A0ValBracket (A1ValConst (A1BITensorZeroGrad ns1))
    BITensorGenSubUpdate -> do
      ns1 <- validateIntListLiteral a0v1
      pure $ A0ValBracket (A1ValConst (A1BITensorSubUpdate ns1))
    BITensorGenCountEqual -> do
      ns1 <- validateIntListLiteral a0v1
      pure $ A0ValBracket (A1ValConst (A1BITensorCountEqual ns1))

reduceDeltaArity2 :: BuiltInArity2 -> Ass0Val -> Ass0Val -> M Ass0Val
reduceDeltaArity2 bi2 a0v1 a0v2 =
  case bi2 of
    BIAdd ->
      arithmetic (\n1 n2 -> A0ValLiteral (ALitInt (n1 + n2)))
    BISub ->
      arithmetic (\n1 n2 -> A0ValLiteral (ALitInt (n1 - n2)))
    BIMult ->
      arithmetic (\n1 n2 -> A0ValLiteral (ALitInt (n1 * n2)))
    BILeq ->
      arithmetic (\n1 n2 -> A0ValLiteral (ALitBool (n1 <= n2)))
    BIAnd ->
      logical (\b1 b2 -> A0ValLiteral (ALitBool (b1 && b2)))
    BIListMap -> do
      a0vsIn <- validateListValue a0v2
      a0vsOut <- mapM (reduceBeta a0v1) a0vsIn
      pure $ A0ValLiteral (ALitList a0vsOut)
    BIGenVconcat ->
      arithmetic (\n1 n2 -> A0ValBracket (A1ValConst (A1BIVconcat n1 n2)))
    BIGenMtranspose ->
      arithmetic (\n1 n2 -> A0ValBracket (A1ValConst (A1BIMtranspose n1 n2)))
    BITensorGenAdd -> do
      ns1 <- validateIntListLiteral a0v1
      ns2 <- validateIntListLiteral a0v2
      pure $ A0ValBracket (A1ValConst (A1BITensorAdd ns1 ns2))
    BIVadd n -> do
      v1 <- validateVec0 a0v1
      v2 <- validateVec0 a0v2
      case Vector.add n v1 v2 of
        Just v -> pure $ A0ValLiteral (ALitVec v)
        Nothing -> bug $ InconsistentAppBuiltInArity2 bi2 a0v1 a0v2
    BIVconcat m n -> do
      v1 <- validateVec0 a0v1
      v2 <- validateVec0 a0v2
      case Vector.concat m n v1 v2 of
        Just v -> pure $ A0ValLiteral (ALitVec v)
        Nothing -> bug $ InconsistentAppBuiltInArity2 bi2 a0v1 a0v2
    BIMconcatVert m1 m2 n -> do
      mat1 <- validateMat0 a0v1
      mat2 <- validateMat0 a0v2
      case Matrix.concatVert m1 m2 n mat1 mat2 of
        Just mat -> pure $ A0ValLiteral (ALitMat mat)
        Nothing -> bug $ InconsistentAppBuiltInArity2 bi2 a0v1 a0v2
    BIDropAt -> do
      n1 <- validateIntLiteral a0v1
      a0vs2 <- validateListValue a0v2
      pure $ A0ValLiteral (ALitList (dropAt n1 a0vs2))
    BIBroadcastable -> do
      ns1 <- validateIntListLiteral a0v1
      ns2 <- validateIntListLiteral a0v2
      let b = isJust (broadcast ns1 ns2)
      pure $ A0ValLiteral (ALitBool b)
    BIBroadcast -> do
      ns1 <- validateIntListLiteral a0v1
      ns2 <- validateIntListLiteral a0v2
      ns <-
        case broadcast ns1 ns2 of
          Just ns' -> pure ns'
          Nothing -> bug $ BroadcastFailed ns1 ns2
      pure $ A0ValLiteral (ALitList (map (A0ValLiteral . ALitInt) ns))
    BIListAppend -> do
      a0vs1 <- validateListValue a0v1
      a0vs2 <- validateListValue a0v2
      pure $ A0ValLiteral (ALitList (a0vs1 ++ a0vs2))
    BIListIter -> do
      a0vsIn <- validateListValue a0v2
      forM_ a0vsIn (reduceBeta a0v1 >=> validateUnitLiteral)
      pure $ A0ValLiteral ALitUnit
    BITensorGenMult -> do
      ns1 <- validateIntListLiteral a0v1
      ns2 <- validateIntListLiteral a0v2
      pure $ A0ValBracket (A1ValConst (A1BITensorMult ns1 ns2))
    BITensorGenArgmax -> do
      ns1 <- validateIntListLiteral a0v1
      n2 <- validateIntLiteral a0v2
      pure $ A0ValBracket (A1ValConst (A1BITensorArgmax ns1 n2))
    BITensorGenCrossEntropyForLogits -> do
      n1 <- validateIntLiteral a0v1
      n2 <- validateIntLiteral a0v2
      pure $ A0ValBracket (A1ValConst (A1BITensorCrossEntropyForLogits n1 n2))
    BITensorAdd ns ->
      case ns of
        [n] -> do
          v1 <- validateVec0 a0v1
          v2 <- validateVec0 a0v2
          case Vector.add n v1 v2 of
            Just v -> pure $ A0ValLiteral (ALitVec v)
            Nothing -> bug $ InconsistentAppBuiltInArity2 bi2 a0v1 a0v2
        [m, n] -> do
          mat1 <- validateMat0 a0v1
          mat2 <- validateMat0 a0v2
          case Matrix.add m n mat1 mat2 of
            Just mat -> pure $ A0ValLiteral (ALitMat mat)
            Nothing -> bug $ InconsistentAppBuiltInArity2 bi2 a0v1 a0v2
        _ ->
          error "TODO: evalExpr0, BITadd, dimension >= 3"
    BITensorMm k m n -> do
      mat1 <- validateMat0 a0v1
      mat2 <- validateMat0 a0v2
      case Matrix.mult k m n mat1 mat2 of
        Just mat -> pure $ A0ValLiteral (ALitMat mat)
        Nothing -> bug $ InconsistentAppBuiltInArity2 bi2 a0v1 a0v2
  where
    arithmetic :: (Int -> Int -> Ass0Val) -> M Ass0Val
    arithmetic f = do
      n1 <- validateIntLiteral a0v1
      n2 <- validateIntLiteral a0v2
      pure (f n1 n2)

    logical :: (Bool -> Bool -> Ass0Val) -> M Ass0Val
    logical f = do
      b1 <- validateBoolLiteral a0v1
      b2 <- validateBoolLiteral a0v2
      pure (f b1 b2)

reduceDeltaArity3 :: BuiltInArity3 -> Ass0Val -> Ass0Val -> Ass0Val -> M Ass0Val
reduceDeltaArity3 bi3 a0v1 a0v2 a0v3 =
  case bi3 of
    BIGenMconcatVert -> do
      n1 <- validateIntLiteral a0v1
      n2 <- validateIntLiteral a0v2
      n3 <- validateIntLiteral a0v3
      pure $ A0ValBracket (A1ValConst (A1BIMconcatVert n1 n2 n3))
    BITensorGenMm -> do
      n1 <- validateIntLiteral a0v1
      n2 <- validateIntLiteral a0v2
      n3 <- validateIntLiteral a0v3
      pure $ A0ValBracket (A1ValConst (A1BITensorMm n1 n2 n3))

reduceDelta :: Ass0PartialBuiltInApp Ass0Val -> Ass0Val -> M Ass0Val
reduceDelta pba a0vArg =
  case pba of
    A0PartialBuiltInApp1With0 bi1 -> reduceDeltaArity1 bi1 a0vArg
    A0PartialBuiltInApp2With0 bi2 -> partial $ A0PartialBuiltInApp2With1 bi2 a0vArg
    A0PartialBuiltInApp2With1 bi2 a0v1 -> reduceDeltaArity2 bi2 a0v1 a0vArg
    A0PartialBuiltInApp3With0 bi3 -> partial $ A0PartialBuiltInApp3With1 bi3 a0vArg
    A0PartialBuiltInApp3With1 bi3 a0v1 -> partial $ A0PartialBuiltInApp3With2 bi3 a0v1 a0vArg
    A0PartialBuiltInApp3With2 bi3 a0v1 a0v2 -> reduceDeltaArity3 bi3 a0v1 a0v2 a0vArg
  where
    partial = pure . A0ValPartialBuiltInApp

reduceBeta :: Ass0Val -> Ass0Val -> M Ass0Val
reduceBeta a0vFun a0vArg =
  case a0vFun of
    A0ValLam Nothing (x, _a0tyv) a0eBody env ->
      evalExpr0
        (Map.insert x (Ass0ValEntry a0vArg) env)
        a0eBody
    A0ValLam (Just (f, _a0tyvRec)) (x, _a0tyv) a0eBody env ->
      evalExpr0
        (Map.insert x (Ass0ValEntry a0vArg) (Map.insert f (Ass0ValEntry a0vFun) env))
        a0eBody
    A0ValPartialBuiltInApp pba ->
      reduceDelta pba a0vArg
    _ ->
      bug $ NotAClosure a0vFun

evalExpr0 :: EvalEnv -> Ass0Expr -> M Ass0Val
evalExpr0 env = \case
  A0Literal lit ->
    A0ValLiteral <$> mapMAssLiteral (evalExpr0 env) lit
  A0Var x ->
    findVal0 env x
  A0BuiltInName bi ->
    pure $
      case bi of
        BuiltInArity1 bi1 -> A0ValPartialBuiltInApp (A0PartialBuiltInApp1With0 bi1)
        BuiltInArity2 bi2 -> A0ValPartialBuiltInApp (A0PartialBuiltInApp2With0 bi2)
        BuiltInArity3 bi3 -> A0ValPartialBuiltInApp (A0PartialBuiltInApp3With0 bi3)
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
  A0Tuple a0e1 a0e2 -> do
    a0v1 <- evalExpr0 env a0e1
    a0v2 <- evalExpr0 env a0e2
    pure $ A0ValTuple a0v1 a0v2
  A0IfThenElse a0e0 a0e1 a0e2 -> do
    a0v0 <- evalExpr0 env a0e0
    b <- validateBoolLiteral a0v0
    if b
      then evalExpr0 env a0e1
      else evalExpr0 env a0e2
  A0Bracket a1e1 -> do
    a1v1 <- evalExpr1 env a1e1
    pure $ A0ValBracket a1v1
  A0TyEqAssert loc ty1eq -> do
    let (a1tye1, a1tye2) = decomposeType1Equation ty1eq
    a1tyv1 <- evalTypeExpr1 env a1tye1
    a1tyv2 <- evalTypeExpr1 env a1tye2
    if a1tyv1 == a1tyv2 -- We can use `==` for stage-1 types
      then
        generateIdentityFunction env (A0TyValCode a1tyv1)
      else do
        EvalState {sourceSpec} <- get
        let spanInFile = getSpanInFile sourceSpec loc
        evalError $ AssertionFailure spanInFile a1tyv1 a1tyv2
  A0RefinementAssert loc a0ePred a0eTarget -> do
    a0vPred <- evalExpr0 env a0ePred
    a0vTarget <- evalExpr0 env a0eTarget
    b <- validateBoolLiteral =<< reduceBeta a0vPred a0vTarget
    if b
      then
        pure a0vTarget
      else do
        EvalState {sourceSpec} <- get
        let spanInFile = getSpanInFile sourceSpec loc
        evalError $ RefinementAssertionFailure spanInFile a0vTarget

evalExpr1 :: EvalEnv -> Ass1Expr -> M Ass1Val
evalExpr1 env = \case
  A1Literal lit ->
    A1ValLiteral <$> mapMAssLiteral (evalExpr1 env) lit
  A1Var x -> do
    symb <- findSymbol env x
    pure $ A1ValVar symb
  A1BuiltInName a1bi ->
    pure $ A1ValConst a1bi
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
  A1Tuple a1e1 a1e2 -> do
    a1v1 <- evalExpr1 env a1e1
    a1v2 <- evalExpr1 env a1e2
    pure $ A1ValTuple a1v1 a1v2
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
  SA0TyPrim a0tyPrim maybePred -> do
    let a0tyValPrim =
          case a0tyPrim of
            A0TyInt -> A0TyValInt
            A0TyFloat -> A0TyValFloat
            A0TyBool -> A0TyValBool
            A0TyUnit -> A0TyValUnit
            A0TyString -> A0TyValString
            A0TyTensor n -> A0TyValTensor n
    maybeVPred <- mapM (evalExpr0 env) maybePred
    pure $ A0TyValPrim a0tyValPrim maybeVPred
  SA0TyList sa0tye1 maybePred -> do
    a0tyv1 <- evalTypeExpr0 env sa0tye1
    maybeVPred <- mapM (evalExpr0 env) maybePred
    pure $ A0TyValList a0tyv1 maybeVPred
  SA0TyProduct sa0tye1 sa0tye2 -> do
    a0tyv1 <- evalTypeExpr0 env sa0tye1
    a0tyv2 <- evalTypeExpr0 env sa0tye2
    pure $ A0TyValProduct a0tyv1 a0tyv2
  SA0TyArrow (xOpt, sa0tye1) sa0tye2 -> do
    a0tyv1 <- evalTypeExpr0 env sa0tye1
    pure $ A0TyValArrow (xOpt, a0tyv1) sa0tye2
  SA0TyCode a1tye1 -> do
    a1tyv1 <- evalTypeExpr1 env a1tye1
    pure $ A0TyValCode a1tyv1

evalTypeExpr1 :: EvalEnv -> Ass1TypeExpr -> M Ass1TypeVal
evalTypeExpr1 env = \case
  A1TyPrim a1tyPrim ->
    A1TyValPrim
      <$> case a1tyPrim of
        A1TyInt -> pure A1TyValInt
        A1TyFloat -> pure A1TyValFloat
        A1TyBool -> pure A1TyValBool
        A1TyUnit -> pure A1TyValUnit
        A1TyString -> pure A1TyValString
        A1TyTensor a0eList -> do
          a0v <- evalExpr0 env a0eList
          a0vs <- validateListValue a0v
          ns <- mapM validateIntLiteral a0vs
          pure $ A1TyValTensor ns
  A1TyList a1tye -> do
    a1tyv <- evalTypeExpr1 env a1tye
    pure $ A1TyValList a1tyv
  A1TyProduct a1tye1 a1tye2 -> do
    a1tyv1 <- evalTypeExpr1 env a1tye1
    a1tyv2 <- evalTypeExpr1 env a1tye2
    pure $ A1TyValProduct a1tyv1 a1tyv2
  A1TyArrow a1tye1 a1tye2 -> do
    a1tyv1 <- evalTypeExpr1 env a1tye1
    a1tyv2 <- evalTypeExpr1 env a1tye2
    pure $ A1TyValArrow a1tyv1 a1tyv2

run :: M a -> EvalState -> Either EvalError a
run = evalStateT

unliftVal :: Ass1Val -> Ass0Expr
unliftVal = \case
  A1ValLiteral lit ->
    A0Literal (mapAssLiteral unliftVal lit)
  A1ValConst a1bi ->
    A0BuiltInName (unliftBuiltInName a1bi)
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
  A1ValTuple a1v1 a1v2 ->
    A0Tuple (unliftVal a1v1) (unliftVal a1v2)
  A1ValIfThenElse a1v0 a1v1 a1v2 ->
    A0IfThenElse (unliftVal a1v0) (unliftVal a1v1) (unliftVal a1v2)

unliftTypeVal :: Ass1TypeVal -> StrictAss0TypeExpr
unliftTypeVal = \case
  A1TyValPrim a1tyvPrim ->
    let a0tyPrim =
          case a1tyvPrim of
            A1TyValInt -> A0TyInt
            A1TyValFloat -> A0TyFloat
            A1TyValBool -> A0TyBool
            A1TyValUnit -> A0TyUnit
            A1TyValString -> A0TyString
            A1TyValTensor ns -> A0TyTensor ns
     in SA0TyPrim a0tyPrim Nothing
  A1TyValList a1tyv ->
    SA0TyList (unliftTypeVal a1tyv) Nothing
  A1TyValProduct a1tyv1 a1tyv2 ->
    SA0TyProduct (unliftTypeVal a1tyv1) (unliftTypeVal a1tyv2)
  A1TyValArrow a1tyv1 a1tyv2 ->
    SA0TyArrow (Nothing, unliftTypeVal a1tyv1) (unliftTypeVal a1tyv2)
