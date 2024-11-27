module Lwsd.Typechecker
  ( typecheckExpr0,
    typecheckExpr1,
    typecheckTypeExpr0,
    typecheckTypeExpr1,
    TypecheckState (..),
    M,
  )
where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Either.Extra
import Data.Function
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Tuple.Extra
import Lwsd.BuiltIn (ass0exprAssertNat)
import Lwsd.SrcSyntax
import Lwsd.Subst
import Lwsd.Syntax
import Lwsd.TypeEnv (TypeEnv)
import Lwsd.TypeEnv qualified as TypeEnv
import Lwsd.TypeError
import Safe.Exact
import Util.LocationInFile (SourceSpec, SpanInFile, getSpanInFile)
import Util.Matrix qualified as Matrix
import Util.TokenUtil (Span)
import Util.Vector qualified as Vector
import Prelude

data TypecheckState = TypecheckState
  { optimizeTrivialAssertion :: Bool,
    sourceSpec :: SourceSpec,
    nextVarIndex :: Int
  }

type M trav a = StateT TypecheckState (Either (TypeError, trav)) a

typeError :: trav -> TypeError -> M trav b
typeError trav e = lift $ Left (e, trav)

bug :: String -> a
bug msg = error $ "bug: " ++ msg

askSpanInFile :: Span -> M trav SpanInFile
askSpanInFile loc = do
  TypecheckState {sourceSpec} <- get
  pure $ getSpanInFile sourceSpec loc

findVar :: trav -> Span -> Var -> TypeEnv -> M trav TypeEnv.Entry
findVar trav loc x tyEnv = do
  spanInFile <- askSpanInFile loc
  lift $ maybeToEither (UnboundVar spanInFile x, trav) $ TypeEnv.findVar x tyEnv

generateFreshVar :: M trav AssVar
generateFreshVar = do
  currentState@TypecheckState {nextVarIndex} <- get
  put $ currentState {nextVarIndex = nextVarIndex + 1}
  pure $ AssVar $ Text.pack $ "#X" ++ show nextVarIndex

makeIdentityLam :: Ass0TypeExpr -> M trav Ass0Expr
makeIdentityLam a0tye = do
  x <- generateFreshVar
  pure $ A0Lam Nothing (x, strictify a0tye) (A0Var x)

applyCast :: Maybe Ass0Expr -> Ass0Expr -> Ass0Expr
applyCast = maybe id A0App

applyCast1 :: Maybe Ass0Expr -> Ass1Expr -> Ass1Expr
applyCast1 cast a1e =
  case cast of
    Nothing -> a1e
    Just a0eCast -> A1Escape (A0App a0eCast (A0Bracket a1e))

applyEquationCast :: Span -> Maybe Type1Equation -> Ass1Expr -> Ass1Expr
applyEquationCast loc eq =
  applyCast1 (A0TyEqAssert loc <$> eq)

-- Returning `Nothing` means there's no need to insert a cast.
makeAssertiveCast :: forall trav. trav -> Span -> Set AssVar -> Ass0TypeExpr -> Ass0TypeExpr -> M trav (Maybe Ass0Expr, InferenceSolution)
makeAssertiveCast trav loc =
  go
  where
    go :: Set AssVar -> Ass0TypeExpr -> Ass0TypeExpr -> M trav (Maybe Ass0Expr, InferenceSolution)
    go _varsToInfer a0tye1 a0tye2
      | alphaEquivalent a0tye1 a0tye2 =
          pure (Nothing, Map.empty)
    go varsToInfer a0tye1 a0tye2 = do
      spanInFile <- askSpanInFile loc
      case (a0tye1, a0tye2) of
        (A0TyPrim a0tyPrim1, A0TyPrim a0tyPrim2) -> do
          case (a0tyPrim1, a0tyPrim2) of
            (A0TyInt, A0TyInt) -> nothingOrIdentityLam (A0TyPrim A0TyInt)
            (A0TyNat, A0TyNat) -> nothingOrIdentityLam (A0TyPrim A0TyNat)
            (A0TyNat, A0TyInt) -> nothingOrIdentityLam (A0TyPrim A0TyInt) -- Implicit upcast
            (A0TyInt, A0TyNat) -> pure (Just (ass0exprAssertNat loc), Map.empty) -- Assertive downcast
            (A0TyBool, A0TyBool) -> nothingOrIdentityLam (A0TyPrim A0TyBool)
            (A0TyTensor ns1, A0TyTensor ns2) ->
              case zipExactMay ns1 ns2 of
                Nothing ->
                  typeError trav $ TypeContradictionAtStage0 spanInFile a0tye1 a0tye2
                Just zipped ->
                  if all (uncurry (==)) zipped
                    then nothingOrIdentityLam (A0TyPrim (A0TyTensor ns1))
                    else typeError trav $ TypeContradictionAtStage0 spanInFile a0tye1 a0tye2
            _ -> typeError trav $ TypeContradictionAtStage0 spanInFile a0tye1 a0tye2
        (A0TyList a0tye1', A0TyList a0tye2') -> do
          go varsToInfer a0tye1' a0tye2'
        (A0TyArrow (x1opt, a0tye11) a0tye12, A0TyArrow (x2opt, a0tye21) a0tye22withX2opt) -> do
          (castDom, solutionDom) <- go varsToInfer a0tye11 a0tye21
          (x, a0tye22) <-
            case (x1opt, x2opt) of
              (Nothing, Nothing) -> do
                x0 <- generateFreshVar
                pure (x0, a0tye22withX2opt)
              (Just x1, Nothing) ->
                pure (x1, a0tye22withX2opt)
              (Nothing, Just x2) ->
                pure (x2, a0tye22withX2opt)
              (Just x1, Just x2) ->
                pure (x1, subst0 (A0Var x1) x2 a0tye22withX2opt)
          (castCod, solutionCod) <-
            go (varsToInfer \\ Map.keysSet solutionDom) (applySolution solutionDom a0tye12) (applySolution solutionDom a0tye22)
          let solution = Map.union solutionDom solutionCod
          cast <-
            makeFunctionTypeCast
              trav
              x
              (applySolution solution a0tye11)
              (applySolution solution a0tye12)
              (applySolution solution a0tye21)
              (applySolution solutionCod <$> castDom)
              castCod
          pure (cast, solution)
        (A0TyOptArrow (x1, a0tye11) a0tye12, A0TyOptArrow (x2, a0tye21) a0tye22withX2) -> do
          (castDom, solutionDom) <- go varsToInfer a0tye11 a0tye21
          let (x, a0tye22) = (x1, subst0 (A0Var x1) x2 a0tye22withX2)
          (castCod, solutionCod) <-
            go (varsToInfer \\ Map.keysSet solutionDom) (applySolution solutionDom a0tye12) (applySolution solutionDom a0tye22)
          let solution = Map.union solutionDom solutionCod
          cast <-
            makeFunctionTypeCast
              trav
              x
              (applySolution solution a0tye11)
              (applySolution solution a0tye12)
              (applySolution solution a0tye21)
              (applySolution solutionCod <$> castDom)
              castCod
          pure (cast, solution)
        (A0TyCode a1tye1, A0TyCode a1tye2) -> do
          (eq, solution) <- makeEquation1 trav loc varsToInfer a1tye1 a1tye2
          pure (A0TyEqAssert loc <$> eq, solution)
        _ ->
          typeError trav $ TypeContradictionAtStage0 spanInFile a0tye1 a0tye2

    makeFunctionTypeCast :: trav -> AssVar -> Ass0TypeExpr -> Ass0TypeExpr -> Ass0TypeExpr -> Maybe Ass0Expr -> Maybe Ass0Expr -> M trav (Maybe Ass0Expr)
    makeFunctionTypeCast _trav x a0tye11 a0tye12 a0tye21 castDom castCod = do
      let a0tye1 = A0TyArrow (Just x, a0tye11) a0tye12
      f <- generateFreshVar
      x' <- generateFreshVar
      pure $
        case (castDom, castCod) of
          (Nothing, Nothing) ->
            Nothing
          _ -> do
            let fDom = applyCast castDom
            let fCod = applyCast castCod
            Just $
              A0Lam Nothing (f, strictify a0tye1) $
                A0Lam Nothing (x, strictify a0tye21) $
                  A0App (A0Lam Nothing (x', strictify a0tye11) (fCod (A0App (A0Var f) (A0Var x')))) (fDom (A0Var x))

    nothingOrIdentityLam :: Ass0TypeExpr -> M trav (Maybe Ass0Expr, InferenceSolution)
    nothingOrIdentityLam a0tye = do
      TypecheckState {optimizeTrivialAssertion} <- get
      if optimizeTrivialAssertion
        then pure (Nothing, Map.empty)
        else (\a0e -> (Just a0e, Map.empty)) <$> makeIdentityLam a0tye

makeEquation1 :: forall trav. trav -> Span -> Set AssVar -> Ass1TypeExpr -> Ass1TypeExpr -> M trav (Maybe Type1Equation, InferenceSolution)
makeEquation1 trav loc varsToInfer' a1tye1' a1tye2' = do
  TypecheckState {optimizeTrivialAssertion} <- get
  spanInFile <- askSpanInFile loc
  case go varsToInfer' a1tye1' a1tye2' of
    Right (trivial, ty1eq, solution) ->
      if trivial && optimizeTrivialAssertion
        then pure (Nothing, solution)
        else pure (Just ty1eq, solution)
    Left () ->
      typeError trav $ TypeContradictionAtStage1 spanInFile a1tye1' a1tye2'
  where
    checkExprArgs :: Set AssVar -> Ass0Expr -> Ass0Expr -> (Bool, Ass0Expr, InferenceSolution)
    checkExprArgs varsToInfer a0e1 a0e2 =
      case a0e2 of
        A0Var x | x `elem` varsToInfer -> (True, a0e1, Map.singleton x a0e1)
        _ -> (alphaEquivalent a0e1 a0e2, a0e2, Map.empty)

    go :: Set AssVar -> Ass1TypeExpr -> Ass1TypeExpr -> Either () (Bool, Type1Equation, InferenceSolution)
    go varsToInfer a1tye1 a1tye2 =
      case (a1tye1, a1tye2) of
        (A1TyPrim a1tyPrim1, A1TyPrim a1tyPrim2) ->
          case (a1tyPrim1, a1tyPrim2) of
            (A1TyInt, A1TyInt) ->
              pure (True, TyEq1Prim TyEq1Int, Map.empty)
            (A1TyBool, A1TyBool) ->
              pure (True, TyEq1Prim TyEq1Bool, Map.empty)
            (A1TyTensor a0es1, A1TyTensor a0es2) -> do
              case zipExactMay a0es1 a0es2 of
                Nothing ->
                  Left ()
                Just zipped -> do
                  let (trivial, equationAccResult, _varsToInfer, solution) =
                        List.foldl'
                          ( \(trivialAcc, equationAcc, varsToInferAcc, solutionAcc) (a0e1, a0e2) ->
                              let a0e1sub = applySolution solutionAcc a0e1
                                  a0e2sub = applySolution solutionAcc a0e2
                                  (trivial', a0e2', solution') = checkExprArgs varsToInferAcc a0e1sub a0e2sub
                               in (trivialAcc && trivial', (a0e1sub, a0e2') : equationAcc, varsToInferAcc \\ Map.keysSet solution', Map.union solution' solutionAcc)
                          )
                          (True, [], varsToInfer, Map.empty)
                          zipped
                  pure (trivial, TyEq1Prim (TyEq1Tensor (reverse equationAccResult)), solution)
            (_, _) ->
              Left ()
        (A1TyArrow a1tye11 a1tye12, A1TyArrow a1tye21 a1tye22) -> do
          (trivial1, ty1eqDom, solution1) <- go varsToInfer a1tye11 a1tye21
          (trivial2, ty1eqCod, solution2) <- go (varsToInfer \\ Map.keysSet solution1) a1tye12 (applySolution solution1 a1tye22)
          pure (trivial1 && trivial2, TyEq1Arrow ty1eqDom ty1eqCod, Map.union solution1 solution2)
        (_, _) ->
          Left ()

mergeTypesByConditional0 :: Ass0Expr -> Ass0TypeExpr -> Ass0TypeExpr -> Either ConditionalMergeError Ass0TypeExpr
mergeTypesByConditional0 a0e0 = go0
  where
    go0 :: Ass0TypeExpr -> Ass0TypeExpr -> Either ConditionalMergeError Ass0TypeExpr
    go0 a0tye1 a0tye2 =
      case (a0tye1, a0tye2) of
        (A0TyPrim a0tyePrim1, A0TyPrim a0tyePrim2) ->
          if a0tyePrim1 == a0tyePrim2
            then pure a0tye1
            else Left $ CannotMerge0 a0tye1 a0tye2
        (A0TyList a0tye1', A0TyList a0tye2') ->
          A0TyList <$> go0 a0tye1' a0tye2'
        (A0TyArrow (x1opt, a0tye11) a0tye12, A0TyArrow (x2opt, a0tye21) a0tye22) -> do
          a0tye1u <- go0 a0tye11 a0tye21
          (xu, a0tye2u) <-
            case (x1opt, x2opt) of
              (Nothing, Nothing) -> (Nothing,) <$> go0 a0tye12 a0tye22
              (Just x1, Nothing) -> (Just x1,) <$> go0 a0tye12 a0tye22
              (Nothing, Just x2) -> (Just x2,) <$> go0 a0tye12 a0tye22
              (Just x1, Just x2) -> (Just x1,) <$> go0 a0tye12 (subst0 (A0Var x1) x2 a0tye22)
          pure $ A0TyArrow (xu, a0tye1u) a0tye2u
        (A0TyCode a1tye1, A0TyCode a1tye2) ->
          A0TyCode <$> go1 a1tye1 a1tye2
        (A0TyOptArrow (x1, a0tye11) a0tye12, A0TyOptArrow (x2, a0tye21) a0tye22) -> do
          a0tye1u <- go0 a0tye11 a0tye21
          a0tye2u <- go0 a0tye12 (subst0 (A0Var x1) x2 a0tye22)
          pure $ A0TyOptArrow (x1, a0tye1u) a0tye2u
        _ ->
          Left $ CannotMerge0 a0tye1 a0tye2

    go1 :: Ass1TypeExpr -> Ass1TypeExpr -> Either ConditionalMergeError Ass1TypeExpr
    go1 = mergeTypesByConditional1 a0e0

mergeTypesByConditional1 :: Ass0Expr -> Ass1TypeExpr -> Ass1TypeExpr -> Either ConditionalMergeError Ass1TypeExpr
mergeTypesByConditional1 a0e0 = go1
  where
    go1 :: Ass1TypeExpr -> Ass1TypeExpr -> Either ConditionalMergeError Ass1TypeExpr
    go1 a1tye1 a1tye2 =
      case (a1tye1, a1tye2) of
        (A1TyPrim a1tyePrim1, A1TyPrim a1tyePrim2) ->
          A1TyPrim
            <$> case (a1tyePrim1, a1tyePrim2) of
              (A1TyInt, A1TyInt) ->
                pure A1TyInt
              (A1TyBool, A1TyBool) ->
                pure A1TyBool
              (A1TyTensor a0es1, A1TyTensor a0es2) ->
                case zipExactMay a0es1 a0es2 of
                  Nothing -> Left $ CannotMerge1 a1tye1 a1tye2
                  Just zipped -> pure $ A1TyTensor (map (uncurry a0branch) zipped)
              _ ->
                Left $ CannotMerge1 a1tye1 a1tye2
        (A1TyArrow a1tye11 a1tye12, A1TyArrow a1tye21 a1tye22) ->
          A1TyArrow <$> go1 a1tye11 a1tye21 <*> go1 a1tye12 a1tye22
        _ ->
          Left $ CannotMerge1 a1tye1 a1tye2

    a0branch = A0IfThenElse a0e0

mergeResultsByConditional0 :: forall trav. trav -> Span -> Ass0Expr -> Result Ass0TypeExpr -> Result Ass0TypeExpr -> M trav (Result Ass0TypeExpr)
mergeResultsByConditional0 trav loc a0e0 = go
  where
    go result1 result2 =
      case (result1, result2) of
        (Pure a0tye1, Pure a0tye2) ->
          Pure <$> mergeTypes0 a0tye1 a0tye2
        (Cast0 cast1 a0tye1 r1, Cast0 cast2 a0tye2 r2) -> do
          a0tye <- mergeTypes0 a0tye1 a0tye2
          cast <- mergeCasts cast1 a0tye1 cast2 a0tye2
          Cast0 cast a0tye <$> go r1 r2
        (Cast1 cast1 a1tye1 r1, Cast1 cast2 a1tye2 r2) -> do
          a1tye <- mergeTypes1 a1tye1 a1tye2
          cast <- mergeCasts cast1 (A0TyCode a1tye1) cast2 (A0TyCode a1tye2)
          Cast1 cast a1tye <$> go r1 r2
        (CastGiven0 cast1 a0tye1 r1, CastGiven0 cast2 a0tye2 r2) -> do
          a0tye <- mergeTypes0 a0tye1 a0tye2
          cast <- mergeCasts cast1 a0tye1 cast2 a0tye2
          CastGiven0 cast a0tye <$> go r1 r2
        (FillInferred0 a0e1 r1, FillInferred0 a0e2 r2) -> do
          FillInferred0 (a0branch a0e1 a0e2) <$> go r1 r2
        (InsertInferred0 a0e1 r1, InsertInferred0 a0e2 r2) -> do
          InsertInferred0 (a0branch a0e1 a0e2) <$> go r1 r2
        _ -> do
          -- Reachable if two branches of an if-expression are inconsistent as to `InsertInferred0`.
          spanInFile <- askSpanInFile loc
          typeError trav $ CannotMergeResultsByConditionals spanInFile result1 result2

    a0branch = A0IfThenElse a0e0

    mergeTypes0 :: Ass0TypeExpr -> Ass0TypeExpr -> M trav Ass0TypeExpr
    mergeTypes0 a0tye1 a0tye2 =
      case mergeTypesByConditional0 a0e0 a0tye1 a0tye2 of
        Left condErr -> do
          spanInFile <- askSpanInFile loc
          typeError trav $ CannotMergeTypesByConditional0 spanInFile a0tye1 a0tye2 condErr
        Right a0tye ->
          pure a0tye

    mergeTypes1 :: Ass1TypeExpr -> Ass1TypeExpr -> M trav Ass1TypeExpr
    mergeTypes1 a1tye1 a1tye2 =
      case mergeTypesByConditional1 a0e0 a1tye1 a1tye2 of
        Left condErr -> do
          spanInFile <- askSpanInFile loc
          typeError trav $ CannotMergeTypesByConditional1 spanInFile a1tye1 a1tye2 condErr
        Right a0tye ->
          pure a0tye

    mergeCasts cast1 a0tye1 cast2 a0tye2 =
      case (cast1, cast2) of
        (Nothing, Nothing) ->
          pure Nothing
        (Just a0e1, Nothing) -> do
          a0e2 <- makeIdentityLam a0tye2
          pure $ Just (a0branch a0e1 a0e2)
        (Nothing, Just a0e2) -> do
          a0e1 <- makeIdentityLam a0tye1
          pure $ Just (a0branch a0e1 a0e2)
        (Just a0e1, Just a0e2) -> do
          pure $ Just (a0branch a0e1 a0e2)

type InferenceSolution = Map AssVar Ass0Expr

applySolution :: forall a. (HasVar a) => InferenceSolution -> a -> a
applySolution solution entity =
  Map.foldrWithKey (flip subst0) entity solution

instantiateGuidedByAppContext0 :: forall trav. trav -> Span -> AppContext -> Ass0TypeExpr -> M trav (Result Ass0TypeExpr)
instantiateGuidedByAppContext0 trav loc appCtx0 a0tye0 = do
  (result, _isubstRet) <- go Set.empty appCtx0 a0tye0
  pure result
  where
    go :: Set AssVar -> AppContext -> Ass0TypeExpr -> M trav (Result Ass0TypeExpr, InferenceSolution)
    go varsToInfer appCtx a0tye =
      case (appCtx, a0tye) of
        ([], _) ->
          pure (Pure a0tye, Map.empty)
        (AppArg0 a0e1' a0tye1' : appCtx', A0TyArrow (xOpt, a0tye1) a0tye2) -> do
          (cast, solution1) <- makeAssertiveCast trav loc varsToInfer a0tye1' a0tye1
          let varsToInfer' = varsToInfer \\ Map.keysSet solution1
          let a0tye2s = applySolution solution1 a0tye2
          (result', solution') <-
            case xOpt of
              Nothing -> go varsToInfer' appCtx' a0tye2s
              Just x -> go varsToInfer' appCtx' (subst0 a0e1' x a0tye2s)
          let solution = Map.union solution1 solution'
          let a0tye1s = applySolution solution a0tye1
          pure (Cast0 (fmap (applySolution solution') cast) a0tye1s result', solution)
        (appCtxEntry : appCtx', A0TyOptArrow (x, a0tye1) a0tye2) ->
          case appCtxEntry of
            AppArgOptGiven0 a0e1' a0tye1' -> do
              (cast, solution1) <- makeAssertiveCast trav loc varsToInfer a0tye1' a0tye1
              let varsToInfer' = varsToInfer \\ Map.keysSet solution1
              let a0tye2s = applySolution solution1 a0tye2
              (result', solution') <- go varsToInfer' appCtx' (subst0 a0e1' x a0tye2s)
              let solution = Map.union solution1 solution'
              let a0tye1s = applySolution solution a0tye1
              pure (CastGiven0 (fmap (applySolution solution') cast) a0tye1s result', solution)
            AppArgOptOmitted0 -> do
              (result', solution') <- go (Set.insert x varsToInfer) appCtx' a0tye2
              a0eInferred <-
                case Map.lookup x solution' of
                  Just a0eInferred' ->
                    pure a0eInferred'
                  Nothing -> do
                    spanInFile <- askSpanInFile loc
                    typeError trav $ CannotInferOptional spanInFile x
              pure (FillInferred0 a0eInferred result', solution')
            _ -> do
              (result', solution') <- go (Set.insert x varsToInfer) appCtx a0tye2
              a0eInferred <-
                case Map.lookup x solution' of
                  Just a0eInferred' ->
                    pure a0eInferred'
                  Nothing -> do
                    spanInFile <- askSpanInFile loc
                    typeError trav $ CannotInferOptional spanInFile x
              pure (InsertInferred0 a0eInferred result', solution')
        (_, A0TyCode a1tye) -> do
          (result', solution) <- instantiateGuidedByAppContext1 trav loc varsToInfer appCtx a1tye
          result <- mapMPure (pure . A0TyCode) result'
          pure (result, solution)
        _ -> do
          spanInFile <- askSpanInFile loc
          typeError trav $ CannotInstantiateGuidedByAppContext0 spanInFile appCtx a0tye

instantiateGuidedByAppContext1 :: forall trav. trav -> Span -> Set AssVar -> AppContext -> Ass1TypeExpr -> M trav (Result Ass1TypeExpr, InferenceSolution)
instantiateGuidedByAppContext1 trav loc = go
  where
    go :: Set AssVar -> AppContext -> Ass1TypeExpr -> M trav (Result Ass1TypeExpr, InferenceSolution)
    go varsToInfer appCtx a1tye =
      case (appCtx, a1tye) of
        ([], _) ->
          pure (Pure a1tye, Map.empty)
        (AppArg1 a1tye1' : appCtx', A1TyArrow a1tye1 a1tye2) -> do
          (eq, solution1) <- makeEquation1 trav loc varsToInfer a1tye1' a1tye1
          (result', solution') <-
            go (varsToInfer \\ Map.keysSet solution1) appCtx' (applySolution solution1 a1tye2)
          let solution = Map.union solution1 solution'
          pure (Cast1 (fmap (applySolution solution' . A0TyEqAssert loc) eq) a1tye1 result', solution)
        _ -> do
          spanInFile <- askSpanInFile loc
          typeError trav $ CannotInstantiateGuidedByAppContext1 spanInFile appCtx a1tye

validateEmptyRetAppContext :: String -> Result a -> M trav a
validateEmptyRetAppContext _ (Pure v) = pure v
validateEmptyRetAppContext msg _ = bug $ "non-empty RetAppContext; " ++ msg

typecheckExpr0 :: trav -> TypeEnv -> AppContext -> Expr -> M trav (Result Ass0TypeExpr, Ass0Expr)
typecheckExpr0 trav tyEnv appCtx (Expr loc eMain) = do
  spanInFile <- askSpanInFile loc
  completeInferredOptional
    <$> case eMain of
      Literal lit ->
        case appCtx of
          [] -> do
            (a0tye, alit) <-
              case lit of
                LitInt n ->
                  pure (A0TyPrim (if n >= 0 then A0TyNat else A0TyInt), ALitInt n)
                LitList es ->
                  case es of
                    [] ->
                      error "TODO: typecheckExpr0, empty list literals"
                    eFirst : esTail -> do
                      (resultFirst, a0eFirst) <- typecheckExpr0 trav tyEnv [] eFirst
                      a0tyeFirst <- validateEmptyRetAppContext "stage-0, LitList, first" resultFirst
                      a0esTail <-
                        mapM
                          ( \e@(Expr locElem _) -> do
                              (result, a0e) <- typecheckExpr0 trav tyEnv [] e
                              a0tye <- validateEmptyRetAppContext "stage-0, LitList, tail" result
                              (cast, _solution) <- makeAssertiveCast trav locElem Set.empty a0tye a0tyeFirst
                              pure $ applyCast cast a0e
                          )
                          esTail
                      pure (A0TyList a0tyeFirst, ALitList (a0eFirst : a0esTail))
                LitVec ns -> do
                  let vec = Vector.fromList ns
                  pure (A0TyPrim (a0TyVec (Vector.length vec)), ALitVec vec)
                LitMat nss -> do
                  mat <- lift . mapLeft (\e -> (InvalidMatrixLiteral spanInFile e, trav)) $ Matrix.fromRows nss
                  pure (A0TyPrim (uncurry a0TyMat (Matrix.size mat)), ALitMat mat)
            pure (Pure a0tye, A0Literal alit)
          _ : _ ->
            typeError trav $ CannotApplyLiteral spanInFile
      Var x -> do
        entry <- findVar trav loc x tyEnv
        let ax = AssVar x
        case entry of
          TypeEnv.Ass0Entry a0tye -> do
            result <- instantiateGuidedByAppContext0 trav loc appCtx a0tye
            pure (result, A0Var ax)
          TypeEnv.Ass1Entry _ ->
            typeError trav $ NotAStage0Var spanInFile x
      Lam recOpt (x1, tye1) e2 ->
        case appCtx of
          [] ->
            case recOpt of
              Nothing -> do
                a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
                (result2, a0e2) <-
                  typecheckExpr0 trav (TypeEnv.addVar x1 (TypeEnv.Ass0Entry a0tye1) tyEnv) [] e2
                a0tye2 <- validateEmptyRetAppContext "stage-0, Lam, non-rec" result2
                let ax1 = AssVar x1
                let sa0tye1 = strictify a0tye1
                pure (Pure (A0TyArrow (Just ax1, a0tye1) a0tye2), A0Lam Nothing (ax1, sa0tye1) a0e2)
              Just (f, tyeRec) -> do
                a0tyeRec <- typecheckTypeExpr0 trav tyEnv tyeRec
                a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
                (result2, a0e2) <- do
                  let tyEnv' =
                        tyEnv
                          & TypeEnv.addVar x1 (TypeEnv.Ass0Entry a0tye1)
                          & TypeEnv.addVar f (TypeEnv.Ass0Entry a0tyeRec)
                  typecheckExpr0 trav tyEnv' [] e2
                a0tye2 <- validateEmptyRetAppContext "stage-0, Lam, rec" result2
                let ax1 = AssVar x1
                let af = AssVar f
                let a0tyeSynth = A0TyArrow (Just ax1, a0tye1) a0tye2
                (cast, _) <- makeAssertiveCast trav loc Set.empty a0tyeSynth a0tyeRec
                let sa0tyeRec = strictify a0tyeRec
                let sa0tye1 = strictify a0tye1
                pure (Pure a0tyeRec, applyCast cast (A0Lam (Just (af, sa0tyeRec)) (ax1, sa0tye1) a0e2))
          _ : _ ->
            error "TODO: stage-0, Lam, non-empty AppContext"
      App e1 e2 -> do
        (result2, a0e2) <- typecheckExpr0 trav tyEnv [] e2
        a0tye2 <- validateEmptyRetAppContext "stage-0, App, arg" result2
        (result1, a0e1) <- typecheckExpr0 trav tyEnv (AppArg0 a0e2 a0tye2 : appCtx) e1
        case result1 of
          Cast0 cast _a0tye11 result -> do
            pure (result, A0App a0e1 (applyCast cast a0e2))
          _ -> do
            bug "stage-0, App, fun"
      LamOpt (x1, tye1) e2 -> do
        case appCtx of
          [] -> do
            a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
            (result2, a0e2) <-
              typecheckExpr0 trav (TypeEnv.addVar x1 (TypeEnv.Ass0Entry a0tye1) tyEnv) [] e2
            a0tye2 <- validateEmptyRetAppContext "stage-1, Lam, non-rec" result2
            let ax1 = AssVar x1
            let sa0tye1 = strictify a0tye1
            pure (Pure (A0TyOptArrow (ax1, a0tye1) a0tye2), A0Lam Nothing (ax1, sa0tye1) a0e2)
          _ : _ ->
            error "TODO: stage-0, LamOpt, non-empty AppContext"
      AppOptGiven e1 e2 -> do
        (result2, a0e2) <- typecheckExpr0 trav tyEnv [] e2
        a0tye2 <- validateEmptyRetAppContext "stage-0, AppOpt, arg" result2
        (result1, a0e1) <- typecheckExpr0 trav tyEnv (AppArgOptGiven0 a0e2 a0tye2 : appCtx) e1
        case result1 of
          CastGiven0 cast _a0tye11 result -> do
            pure (result, A0App a0e1 (applyCast cast a0e2))
          _ -> do
            bug "stage-0, AppOptGiven, not a CastGiven0"
      AppOptOmitted e1 -> do
        (result1, a0e1) <- typecheckExpr0 trav tyEnv (AppArgOptOmitted0 : appCtx) e1
        case result1 of
          FillInferred0 a0eInferred result -> do
            pure (result, A0App a0e1 a0eInferred)
          _ -> do
            bug "stage-0, AppOptOmitted, not a FillInferred0"
      LetIn x e1 e2 -> do
        (result1, a0e1) <- typecheckExpr0 trav tyEnv [] e1
        a0tye1 <- validateEmptyRetAppContext "stage-0, LetIn" result1
        (result2, a0e2) <-
          typecheckExpr0 trav (TypeEnv.addVar x (TypeEnv.Ass0Entry a0tye1) tyEnv) appCtx e2
        let ax = AssVar x
        let sa0tye1 = strictify a0tye1
        if ax `occurs0` result2
          then typeError trav $ VarOccursFreelyInAss0Type spanInFile x result2
          else pure (result2, A0LetIn (ax, sa0tye1) a0e1 a0e2)
      IfThenElse e0 e1 e2 -> do
        (result0, a0e0) <- typecheckExpr0 trav tyEnv [] e0
        a0tye0 <- validateEmptyRetAppContext "stage-0, IfThenElse, condition" result0
        case a0tye0 of
          A0TyPrim A0TyBool -> do
            (result1, a0e1) <- typecheckExpr0 trav tyEnv appCtx e1
            (result2, a0e2) <- typecheckExpr0 trav tyEnv appCtx e2
            result <- mergeResultsByConditional0 trav loc a0e0 result1 result2
            pure (result, A0IfThenElse a0e0 a0e1 a0e2)
          _ -> do
            let Expr loc0 _ = e0
            spanInFile0 <- askSpanInFile loc0
            typeError trav $ NotABoolTypeForStage0 spanInFile0 a0tye0
      As e1 tye2 -> do
        -- `e1 as τ2` is treated in the same way as `(λx : τ2. x) e1`.
        (result1, a0e1) <- typecheckExpr0 trav tyEnv [] e1
        a0tye1 <- validateEmptyRetAppContext "stage-0, As, 1" result1
        a0tye2 <- typecheckTypeExpr0 trav tyEnv tye2
        result' <- instantiateGuidedByAppContext0 trav loc (AppArg0 a0e1 a0tye1 : appCtx) a0tye2
        case result' of
          Cast0 cast _a0tye' result ->
            pure (result, applyCast cast a0e1)
          _ ->
            bug "stage-0, As"
      Bracket e1 -> do
        (result1, a1e1) <- typecheckExpr1 trav tyEnv appCtx e1
        result <- mapMPure (pure . A0TyCode) result1
        pure (result, A0Bracket a1e1)
      Escape _ ->
        typeError trav $ CannotUseEscapeAtStage0 spanInFile
  where
    completeInferredOptional pair@(result, a0e) =
      case result of
        InsertInferred0 a0eInferred result' ->
          completeInferredOptional (result', A0App a0e a0eInferred)
        _ ->
          pair

typecheckExpr1 :: trav -> TypeEnv -> AppContext -> Expr -> M trav (Result Ass1TypeExpr, Ass1Expr)
typecheckExpr1 trav tyEnv appCtx (Expr loc eMain) = do
  spanInFile <- askSpanInFile loc
  case eMain of
    Literal lit ->
      case appCtx of
        [] -> do
          (a1tye, alit) <-
            case lit of
              LitInt n ->
                pure (A1TyPrim A1TyInt, ALitInt n)
              LitList es ->
                case es of
                  [] ->
                    error "TODO: typecheckExpr1, empty list literal"
                  eFirst : esTail -> do
                    (resultFirst, a1eFirst) <- typecheckExpr1 trav tyEnv [] eFirst
                    a1tyeFirst <- validateEmptyRetAppContext "stage-1, LitList, first" resultFirst
                    a1esTail <-
                      mapM
                        ( \e@(Expr locElem _) -> do
                            (result, a1e) <- typecheckExpr1 trav tyEnv [] e
                            a1tye <- validateEmptyRetAppContext "stage-1, LitList, tail" result
                            (eq, _solution) <- makeEquation1 trav locElem Set.empty a1tye a1tyeFirst
                            pure (applyEquationCast locElem eq a1e)
                        )
                        esTail
                    pure (A1TyList a1tyeFirst, ALitList (a1eFirst : a1esTail))
              LitVec ns -> do
                let vec = Vector.fromList ns
                pure (A1TyPrim (a1TyVec (A0Literal (ALitInt (Vector.length vec)))), ALitVec vec)
              LitMat nss -> do
                mat <- lift . mapLeft (\e -> (InvalidMatrixLiteral spanInFile e, trav)) $ Matrix.fromRows nss
                pure (A1TyPrim (uncurry a1TyMat (both (A0Literal . ALitInt) (Matrix.size mat))), ALitMat mat)
          pure (Pure a1tye, A1Literal alit)
        _ : _ ->
          typeError trav $ CannotApplyLiteral spanInFile
    Var x -> do
      entry <- findVar trav loc x tyEnv
      let ax = AssVar x
      case entry of
        TypeEnv.Ass0Entry _ ->
          typeError trav $ NotAStage1Var spanInFile x
        TypeEnv.Ass1Entry a1tye -> do
          (result, _) <- instantiateGuidedByAppContext1 trav loc Set.empty appCtx a1tye
          pure (result, A1Var ax)
    Lam recOpt (x1, tye1) e2 ->
      case appCtx of
        [] ->
          case recOpt of
            Nothing -> do
              a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
              (result2, a1e2) <-
                typecheckExpr1 trav (TypeEnv.addVar x1 (TypeEnv.Ass1Entry a1tye1) tyEnv) [] e2
              a1tye2 <- validateEmptyRetAppContext "stage-1, Lam, non-rec" result2
              let ax1 = AssVar x1
              pure (Pure (A1TyArrow a1tye1 a1tye2), A1Lam Nothing (ax1, a1tye1) a1e2)
            Just (f, tyeRec) -> do
              a1tyeRec <- typecheckTypeExpr1 trav tyEnv tyeRec
              a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
              (result2, a1e2) <- do
                let tyEnv' =
                      tyEnv
                        & TypeEnv.addVar x1 (TypeEnv.Ass1Entry a1tye1)
                        & TypeEnv.addVar f (TypeEnv.Ass1Entry a1tyeRec)
                typecheckExpr1 trav tyEnv' [] e2
              a1tye2 <- validateEmptyRetAppContext "stage-1, Lam, rec" result2
              let ax1 = AssVar x1
              let af = AssVar f
              let a1tyeSynth = A1TyArrow a1tye1 a1tye2
              (eq, _) <- makeEquation1 trav loc Set.empty a1tyeSynth a1tyeRec
              pure (Pure a1tyeRec, applyEquationCast loc eq (A1Lam (Just (af, a1tyeRec)) (ax1, a1tye1) a1e2))
        _ : _ ->
          error "TODO: stage-1, Lam, non-empty AppContext"
    App e1 e2 -> do
      (result2, a1e2) <- typecheckExpr1 trav tyEnv [] e2
      a1tye2 <- validateEmptyRetAppContext "stage-1, App, arg" result2
      (result1, a1e1) <- typecheckExpr1 trav tyEnv (AppArg1 a1tye2 : appCtx) e1
      case result1 of
        Cast1 cast _a1tye11 result ->
          -- Embeds type equality assertion at stage 0 here!
          pure (result, A1App a1e1 (applyCast1 cast a1e2))
        _ ->
          bug "stage-1, App, fun, not a Cast1"
    LamOpt _ _ ->
      typeError trav $ CannotUseLamOptAtStage1 spanInFile
    AppOptGiven _ _ ->
      typeError trav $ CannotUseAppOptGivenAtStage1 spanInFile
    AppOptOmitted _ ->
      typeError trav $ CannotUseAppOptOmittedAtStage1 spanInFile
    LetIn x e1 e2 -> do
      (result1, a1e1) <- typecheckExpr1 trav tyEnv [] e1
      a1tye1 <- validateEmptyRetAppContext "stage-1, LetIn" result1
      (result2, a1e2) <-
        typecheckExpr1 trav (TypeEnv.addVar x (TypeEnv.Ass1Entry a1tye1) tyEnv) appCtx e2
      let ax = AssVar x
      if ax `occurs0` result2
        then typeError trav $ VarOccursFreelyInAss1Type spanInFile x result2
        else pure (result2, A1App (A1Lam Nothing (ax, a1tye1) a1e2) a1e1)
    IfThenElse e0 e1 e2 -> do
      (result0, a1e0) <- typecheckExpr1 trav tyEnv [] e0
      a1tye0 <- validateEmptyRetAppContext "stage-1, IfThenElse, condition" result0
      case a1tye0 of
        A1TyPrim A1TyBool ->
          case appCtx of
            [] -> do
              (result1, a1e1) <- typecheckExpr1 trav tyEnv appCtx e1
              a1tye1 <- validateEmptyRetAppContext "stage-1, IfThenElse, then" result1
              (result2, a1e2) <- typecheckExpr1 trav tyEnv appCtx e2
              a1tye2 <- validateEmptyRetAppContext "stage-1, IfThenElse, else" result2
              (eq, _) <- makeEquation1 trav loc Set.empty a1tye2 a1tye1
              pure (Pure a1tye1, A1IfThenElse a1e0 a1e1 (applyEquationCast loc eq a1e2))
            _ : _ -> do
              typeError trav $ Stage1IfThenElseRestrictedToEmptyContext spanInFile appCtx
        _ -> do
          let Expr loc0 _ = e0
          spanInFile0 <- askSpanInFile loc0
          typeError trav $ NotABoolTypeForStage1 spanInFile0 a1tye0
    As e1 tye2 -> do
      -- `e1 as τ2` is treated in the same way as `(λx : τ2. x) e1`.
      (result1, a1e1) <- typecheckExpr1 trav tyEnv [] e1
      a1tye1 <- validateEmptyRetAppContext "stage-1, As" result1
      a1tye2 <- typecheckTypeExpr1 trav tyEnv tye2
      (result', _) <- instantiateGuidedByAppContext1 trav loc Set.empty (AppArg1 a1tye1 : appCtx) a1tye2
      case result' of
        Cast1 cast _a1tye' result -> do
          pure (result, applyCast1 cast a1e1)
        _ ->
          bug "stage-1, As"
    Bracket _ ->
      typeError trav $ CannotUseBracketAtStage1 spanInFile
    Escape e1 -> do
      (result1, a0e1) <- typecheckExpr0 trav tyEnv appCtx e1
      result <-
        mapMPure
          ( \case
              A0TyCode a1tye1 ->
                pure a1tye1
              a0tye1 -> do
                let Expr loc1 _ = e1
                spanInFile1 <- askSpanInFile loc1
                typeError trav $ NotACodeType spanInFile1 a0tye1
          )
          result1
      pure (result, A1Escape a0e1)

mapMPure :: (a -> M trav b) -> Result a -> M trav (Result b)
mapMPure f = go
  where
    go (Pure v) = Pure <$> f v
    go (Cast0 cast a0tye r) = Cast0 cast a0tye <$> go r
    go (Cast1 eq a1tye r) = Cast1 eq a1tye <$> go r
    go (CastGiven0 a0e a0tye r) = CastGiven0 a0e a0tye <$> go r
    go (FillInferred0 a0e r) = FillInferred0 a0e <$> go r
    go (InsertInferred0 a0e r) = InsertInferred0 a0e <$> go r

insertCastForNatArg :: trav -> (Ass0TypeExpr, Ass0Expr, Span) -> M trav Ass0Expr
insertCastForNatArg trav (a0tye, a0e, loc) = do
  (cast, _) <- makeAssertiveCast trav loc Set.empty a0tye (A0TyPrim A0TyNat)
  pure $ applyCast cast a0e

validateIntLiteral :: trav -> (IntermediateArgForAss0Type, Span) -> M trav Int
validateIntLiteral trav = \case
  (IA0ExprArg (A0Literal (ALitInt n)), _) ->
    pure n
  (IA0ExprArg a0e, loc) -> do
    spanInFile <- askSpanInFile loc
    typeError trav $ NotAnIntLitArgAtStage0 spanInFile a0e
  (IA0TypeArg _a0tye, _loc) -> do
    error "TODO (error): validateIntLiteral, IA0TypeArg"

data IntermediateArgForAss0Type
  = IA0ExprArg Ass0Expr
  | IA0TypeArg Ass0TypeExpr

typecheckTypeExpr0 :: trav -> TypeEnv -> TypeExpr -> M trav Ass0TypeExpr
typecheckTypeExpr0 trav tyEnv (TypeExpr loc tyeMain) = do
  spanInFile <- askSpanInFile loc
  case tyeMain of
    TyName tyName args -> do
      results <-
        mapM
          ( \case
              ExprArgPersistent (Expr loc' _) -> do
                spanInFile' <- askSpanInFile loc'
                typeError trav $ CannotUsePersistentArgAtStage0 spanInFile'
              ExprArgNormal e@(Expr loc' _) -> do
                (result, a0e) <- typecheckExpr0 trav tyEnv [] e
                _a0tye <- validateEmptyRetAppContext "NormalArg" result
                pure (IA0ExprArg a0e, loc')
              TypeArg tye@(TypeExpr loc' _) -> do
                a0tye <- typecheckTypeExpr0 trav tyEnv tye
                pure (IA0TypeArg a0tye, loc')
          )
          args
      case (tyName, results) of
        ("Int", []) -> pure $ A0TyPrim A0TyInt
        ("Nat", []) -> pure $ A0TyPrim A0TyNat
        ("Bool", []) -> pure $ A0TyPrim A0TyBool
        ("List", [arg]) -> do
          case arg of
            (IA0TypeArg a0tye, _) -> pure $ A0TyList a0tye
            (IA0ExprArg _a0e, _loc) -> error "TODO (error): typecheckTypeExpr0, TyName, List, IA0ExprArg"
        ("Vec", [arg]) -> do
          n <- validateIntLiteral trav arg
          pure $ A0TyPrim (a0TyVec n)
        ("Mat", [arg1, arg2]) -> do
          m <- validateIntLiteral trav arg1
          n <- validateIntLiteral trav arg2
          pure $ A0TyPrim (a0TyMat m n)
        ("Tensor", _) -> do
          error "TODO: typecheckTypeExpr0, Tensor"
        _ -> typeError trav $ UnknownTypeOrInvalidArityAtStage0 spanInFile tyName (List.length results)
    TyArrow (xOpt, tye1) tye2 -> do
      a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
      a0tye2 <-
        case xOpt of
          Just x -> typecheckTypeExpr0 trav (TypeEnv.addVar x (TypeEnv.Ass0Entry a0tye1) tyEnv) tye2
          Nothing -> typecheckTypeExpr0 trav tyEnv tye2
      let axOpt = fmap AssVar xOpt
      pure $ A0TyArrow (axOpt, a0tye1) a0tye2
    TyCode tye1 -> do
      a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
      pure $ A0TyCode a1tye1
    TyOptArrow (x, tye1) tye2 -> do
      a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
      a0tye2 <-
        typecheckTypeExpr0 trav (TypeEnv.addVar x (TypeEnv.Ass0Entry a0tye1) tyEnv) tye2
      let ax = AssVar x
      pure $ A0TyOptArrow (ax, a0tye1) a0tye2

data IntermediateArgForAss1Type
  = IA1ExprArg Ass0Expr Ass0TypeExpr
  | IA1TypeArg Ass1TypeExpr

typecheckTypeExpr1 :: trav -> TypeEnv -> TypeExpr -> M trav Ass1TypeExpr
typecheckTypeExpr1 trav tyEnv (TypeExpr loc tyeMain) = do
  spanInFile <- askSpanInFile loc
  case tyeMain of
    TyName tyName args -> do
      results <-
        mapM
          ( \case
              ExprArgPersistent e@(Expr loc' _) -> do
                (result, a0e) <- typecheckExpr0 trav tyEnv [] e
                a0tye <- validateEmptyRetAppContext "PersistentArg" result
                pure (IA1ExprArg a0e a0tye, loc')
              ExprArgNormal (Expr loc' _) -> do
                spanInFile' <- askSpanInFile loc'
                typeError trav $ CannotUseNormalArgAtStage1 spanInFile'
              TypeArg tye@(TypeExpr loc' _) -> do
                a1tye <- typecheckTypeExpr1 trav tyEnv tye
                pure (IA1TypeArg a1tye, loc')
          )
          args
      case (tyName, results) of
        ("Int", []) -> pure $ A1TyPrim A1TyInt
        ("Bool", []) -> pure $ A1TyPrim A1TyBool
        ("List", [(IA1TypeArg a1tye, _)]) -> do
          pure $ A1TyList a1tye
        ("Vec", [(IA1ExprArg a0e' a0tye, loc')]) -> do
          a0e <- insertCastForNatArg trav (a0tye, a0e', loc')
          pure $ A1TyPrim (a1TyVec a0e)
        ("Mat", [(IA1ExprArg a0e1' a0tye1, loc1), (IA1ExprArg a0e2' a0tye2, loc2)]) -> do
          a0e1 <- insertCastForNatArg trav (a0tye1, a0e1', loc1)
          a0e2 <- insertCastForNatArg trav (a0tye2, a0e2', loc2)
          pure $ A1TyPrim (a1TyMat a0e1 a0e2)
        ("Tensor", _) -> do
          error "TODO: typecheckTypeExpr1, Tensor"
        _ -> typeError trav $ UnknownTypeOrInvalidArityAtStage1 spanInFile tyName (List.length results)
    TyArrow (xOpt, tye1) tye2 -> do
      a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
      () <-
        case xOpt of
          Just x -> typeError trav $ FunctionTypeCannotBeDependentAtStage1 spanInFile x
          Nothing -> pure ()
      a1tye2 <- typecheckTypeExpr1 trav tyEnv tye2
      pure $ A1TyArrow a1tye1 a1tye2
    TyOptArrow _ _ ->
      typeError trav $ CannotUseOptArrowTypeAtStage1 spanInFile
    TyCode _ -> do
      typeError trav $ CannotUseCodeTypeAtStage1 spanInFile
