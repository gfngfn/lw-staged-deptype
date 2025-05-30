module Lwsd.Typechecker
  ( typecheckExpr0,
    typecheckExpr1,
    typecheckTypeExpr0,
    typecheckTypeExpr1,
    typecheckBind,
    typecheckBinds,
    TypecheckConfig (..),
    TypecheckState (..),
    M,
    run,
  )
where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Either.Extra
import Data.Foldable (foldrM)
import Data.Function
import Data.List qualified as List
import Data.List.Extra qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tuple.Extra
import Lwsd.BuiltIn qualified as BuiltIn
import Lwsd.BuiltIn.Core
import Lwsd.Scope.SigRecord (Ass0Metadata (..), Ass1Metadata (..), AssPersMetadata (..), ModuleEntry (..), SigRecord, ValEntry (..))
import Lwsd.Scope.SigRecord qualified as SigRecord
import Lwsd.Scope.TypeEnv (TypeEnv, TypeVarEntry (..))
import Lwsd.Scope.TypeEnv qualified as TypeEnv
import Lwsd.SrcSyntax
import Lwsd.Subst
import Lwsd.Syntax
import Lwsd.TypeError
import Safe.Exact
import Util.LocationInFile (SourceSpec, SpanInFile, getSpanInFile)
import Util.Matrix qualified as Matrix
import Util.TokenUtil (Span)
import Util.Vector qualified as Vector
import Prelude

data TypecheckConfig = TypecheckConfig
  { optimizeTrivialAssertion :: Bool,
    distributeIfUnderTensorShape :: Bool,
    sourceSpec :: SourceSpec
  }

data TypecheckState = TypecheckState
  { nextVarIndex :: Int,
    assVarDisplay :: Map StaticVar Text,
    nextTypeVarIndex :: Int,
    assTypeVarDisplay :: Map AssTypeVar Text
  }

type MImpl err trav a = StateT TypecheckState (Reader TypecheckConfig) (Either (err, trav) a)

newtype M' err trav a = M' (MImpl err trav a)

mapRightOfM :: (a -> MImpl err trav b) -> Either (err, trav) a -> MImpl err trav b
mapRightOfM f = \case
  Left e -> lift $ pure $ Left e
  Right vx -> f vx

flipFmapImpl :: MImpl err trav a -> (a -> b) -> MImpl err trav b
flipFmapImpl sx vf = sx >>= mapRightOfM (pure . Right . vf)

instance Functor (M' err trav) where
  fmap vf (M' sx) = M' $ flipFmapImpl sx vf

instance Applicative (M' err trav) where
  pure v = M' $ pure $ Right v
  (M' sf) <*> (M' sx) = M' $ sf >>= mapRightOfM (flipFmapImpl sx)

instance Monad (M' err trav) where
  (M' sx) >>= f = M' $ sx >>= mapRightOfM (\v -> let M' s' = f v in s')

type M trav a = M' TypeError trav a

getState :: M' err trav TypecheckState
getState = M' $ Right <$> get

putState :: TypecheckState -> M' err trav ()
putState tcState = M' $ Right <$> put tcState

askConfig :: M trav TypecheckConfig
askConfig = M' $ lift $ Right <$> ask

typeError :: trav -> err -> M' err trav a
typeError trav e = M' $ lift $ pure $ Left (e, trav)

mapTypeError :: (err1 -> err2) -> M' err1 trav a -> M' err2 trav a
mapTypeError f (M' s) = M' $ mapStateT (mapReader (first (mapLeft (first f)))) s

bug :: String -> a
bug msg = error $ "bug: " ++ msg

askSpanInFile :: Span -> M trav SpanInFile
askSpanInFile loc = do
  TypecheckConfig {sourceSpec} <- askConfig
  pure $ getSpanInFile sourceSpec loc

liftEither :: Either (TypeError, trav) a -> M trav a
liftEither x = M' $ pure x

findValVar :: trav -> Span -> [Var] -> Var -> TypeEnv -> M trav ValEntry
findValVar trav loc ms x tyEnv = do
  spanInFile <- askSpanInFile loc
  liftEither $ maybeToEither (UnboundVar spanInFile ms x, trav) $ do
    case ms of
      [] ->
        TypeEnv.findVal x tyEnv
      m : ms' -> do
        ModuleEntry sigr <- TypeEnv.findModule m tyEnv
        go sigr ms'
  where
    go :: SigRecord -> [Var] -> Maybe ValEntry
    go sigr [] =
      SigRecord.findVal x sigr
    go sigr (m : ms') = do
      ModuleEntry sigr' <- SigRecord.findModule m sigr
      go sigr' ms'

findTypeVar :: trav -> Span -> TypeVar -> TypeEnv -> M trav TypeVarEntry
findTypeVar trav loc tyvar tyEnv = do
  spanInFile <- askSpanInFile loc
  case TypeEnv.findTypeVar tyvar tyEnv of
    Nothing ->
      typeError trav $ UnboundTypeVar spanInFile tyvar
    Just tyVarEntry ->
      pure tyVarEntry

generateFreshVar :: Maybe Text -> M' err trav StaticVar
generateFreshVar maybeName = do
  currentState@TypecheckState {nextVarIndex = n, assVarDisplay} <- getState
  let t = fromMaybe (Text.pack ("#X" ++ show n)) maybeName
  let sv = StaticVar n
  putState $ currentState {nextVarIndex = n + 1, assVarDisplay = Map.insert sv t assVarDisplay}
  pure sv

generateFreshTypeVar :: TypeVar -> M' err trav AssTypeVar
generateFreshTypeVar (TypeVar name) = do
  currentState@TypecheckState {nextTypeVarIndex = n, assTypeVarDisplay} <- getState
  let atyvar = AssTypeVar n
  putState $
    currentState
      { nextTypeVarIndex = n + 1,
        assTypeVarDisplay = Map.insert atyvar name assTypeVarDisplay
      }
  pure atyvar

makeIdentityLam :: Ass0TypeExpr -> M trav Ass0Expr
makeIdentityLam a0tye = do
  sv <- generateFreshVar Nothing
  let ax = AssVarStatic sv
  pure $ A0Lam Nothing (ax, strictify a0tye) (A0Var ax)

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

-- The core part of the cast insertion for stage 0.
-- Returning `(Nothing, ...)` means there's no need to insert a cast.
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
        (A0TyPrim a0tyPrim1 maybePred1, A0TyPrim a0tyPrim2 maybePred2') -> do
          -- Ad hoc optimization of refinement cast insertion:
          let maybePred2 =
                if alphaEquivalent (Maybe1 maybePred2') (Maybe1 maybePred1)
                  then Nothing
                  else maybePred2'
          cast <-
            case (a0tyPrim1, a0tyPrim2) of
              (A0TyPrimBase tyPrimBase1, A0TyPrimBase tyPrimBase2) ->
                if tyPrimBase1 == tyPrimBase2
                  then castOrIdentityLam maybePred2 (A0TyPrim (A0TyPrimBase tyPrimBase1) maybePred1)
                  else typeError trav $ TypeContradictionAtStage0 spanInFile a0tye1 a0tye2
              (A0TyTensor ns1, A0TyTensor ns2) ->
                case zipExactMay ns1 ns2 of
                  Nothing ->
                    typeError trav $ TypeContradictionAtStage0 spanInFile a0tye1 a0tye2
                  Just zipped ->
                    if all (uncurry (==)) zipped
                      then castOrIdentityLam maybePred2 (A0TyPrim (A0TyTensor ns1) maybePred1)
                      else typeError trav $ TypeContradictionAtStage0 spanInFile a0tye1 a0tye2
              _ -> typeError trav $ TypeContradictionAtStage0 spanInFile a0tye1 a0tye2
          pure (cast, Map.empty)
        (A0TyList a0tye1' maybePred1, A0TyList a0tye2' maybePred2') -> do
          (castForElem, solution) <- go varsToInfer a0tye1' a0tye2'
          -- Ad hoc optimization of refinement cast insertion:
          let maybePred2 =
                if alphaEquivalent (Maybe1 maybePred2') (Maybe1 maybePred1)
                  then Nothing
                  else maybePred2'
          let castForListByElemPred =
                case castForElem of
                  Nothing -> Nothing
                  Just a0eCastForElem -> Just (A0App ass0exprListMap a0eCastForElem)
          castForListByWholePred <- castOrIdentityLam maybePred2 a0tye1
          castForList <-
            case (castForListByElemPred, castForListByWholePred) of
              (Nothing, Nothing) ->
                pure Nothing
              (Just a0eCast1, Nothing) ->
                pure $ Just a0eCast1
              (Nothing, Just a0eCast2) ->
                pure $ Just a0eCast2
              (Just a0eCast1, Just a0eCast2) -> do
                sv <- generateFreshVar Nothing
                let ax = AssVarStatic sv
                pure $
                  Just $
                    A0Lam Nothing (ax, strictify a0tye1) $
                      A0App (A0App ass0exprAnd (A0App a0eCast1 (A0Var ax))) (A0App a0eCast2 (A0Var ax))
          pure (castForList, solution)
        (A0TyArrow (x1opt, a0tye11) a0tye12, A0TyArrow (x2opt, a0tye21) a0tye22withX2opt) -> do
          (castDom, solutionDom) <- go varsToInfer a0tye11 a0tye21
          (x, a0tye22) <-
            case (x1opt, x2opt) of
              (Nothing, Nothing) -> do
                sv <- generateFreshVar Nothing
                let x0 = AssVarStatic sv
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
      f <- AssVarStatic <$> generateFreshVar Nothing
      x' <- AssVarStatic <$> generateFreshVar Nothing
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

    castOrIdentityLam :: Maybe Ass0Expr -> Ass0TypeExpr -> M trav (Maybe Ass0Expr)
    castOrIdentityLam maybePred2 a0tye1 = do
      TypecheckConfig {optimizeTrivialAssertion} <- askConfig
      case maybePred2 of
        Nothing ->
          if optimizeTrivialAssertion
            then pure Nothing
            else Just <$> makeIdentityLam a0tye1
        Just a0ePred2 -> do
          x <- AssVarStatic <$> generateFreshVar Nothing
          pure $ Just (A0Lam Nothing (x, strictify a0tye1) (A0RefinementAssert loc a0ePred2 (A0Var x)))

-- The core part of the cast insertion for stage 1.
makeEquation1 :: forall trav. trav -> Span -> Set AssVar -> Ass1TypeExpr -> Ass1TypeExpr -> M trav (Maybe Type1Equation, InferenceSolution)
makeEquation1 trav loc varsToInfer' a1tye1' a1tye2' = do
  TypecheckConfig {optimizeTrivialAssertion} <- askConfig
  spanInFile <- askSpanInFile loc
  case go varsToInfer' a1tye1' a1tye2' of
    Right (trivial, ty1eq, solution) ->
      if trivial && optimizeTrivialAssertion
        then pure (Nothing, solution)
        else pure (Just ty1eq, solution)
    Left () ->
      typeError trav $ TypeContradictionAtStage1 spanInFile a1tye1' a1tye2'
  where
    checkExprArgs :: Set AssVar -> (Ass0Expr, Ass0TypeExpr) -> Ass0Expr -> (Bool, Ass0Expr, InferenceSolution)
    checkExprArgs varsToInfer (a0e1, a0tye1) a0e2 =
      case a0e2 of
        A0Var x | x `elem` varsToInfer -> (True, a0e1, Map.singleton x (a0e1, a0tye1))
        _ -> (alphaEquivalent a0e1 a0e2, a0e2, Map.empty)

    go :: Set AssVar -> Ass1TypeExpr -> Ass1TypeExpr -> Either () (Bool, Type1Equation, InferenceSolution)
    go varsToInfer a1tye1 a1tye2 =
      case (a1tye1, a1tye2) of
        (A1TyPrim a1tyPrim1, A1TyPrim a1tyPrim2) ->
          case (a1tyPrim1, a1tyPrim2) of
            (A1TyPrimBase tyPrimBase1, A1TyPrimBase tyPrimBase2) ->
              if tyPrimBase1 == tyPrimBase2
                then pure (True, TyEq1Prim (TyEq1PrimBase tyPrimBase1), Map.empty)
                else Left ()
            (A1TyTensor a0eList1, A1TyTensor a0eList2) -> do
              case (a0eList1, a0eList2) of
                -- Enhancement for the argument inference 1:
                (A0Literal (ALitList a0es1), A0Literal (ALitList a0es2)) ->
                  case zipExactMay a0es1 a0es2 of
                    Nothing ->
                      Left ()
                    Just zipped -> do
                      let (trivial, equationAccResult, _varsToInfer, solution) =
                            List.foldl'
                              ( \(trivialAcc, equationAcc, varsToInferAcc, solutionAcc) (a0e1, a0e2) ->
                                  let a0e1sub = applySolution solutionAcc a0e1
                                      a0e2sub = applySolution solutionAcc a0e2
                                      (trivial', a0e2', solution') =
                                        checkExprArgs varsToInferAcc (a0e1sub, BuiltIn.tyNat) a0e2sub
                                   in (trivialAcc && trivial', (a0e1sub, a0e2') : equationAcc, varsToInferAcc \\ Map.keysSet solution', Map.union solution' solutionAcc)
                              )
                              (True, [], varsToInfer, Map.empty)
                              zipped
                      pure (trivial, TyEq1Prim (TyEq1TensorByLiteral (reverse equationAccResult)), solution)
                -- Enhancement for the argument inference 2:
                (_, A0Var x2)
                  | x2 `elem` varsToInfer -> do
                      let solution = Map.singleton x2 (a0eList1, A0TyList BuiltIn.tyNat Nothing)
                      pure (True, TyEq1Prim (TyEq1TensorByWhole a0eList1 a0eList1), solution)
                -- General rule:
                (_, _) -> do
                  let trivial = alphaEquivalent a0eList1 a0eList2
                  pure (trivial, TyEq1Prim (TyEq1TensorByWhole a0eList1 a0eList2), Map.empty)
            (_, _) ->
              Left ()
        (A1TyList a1tye1elem, A1TyList a1tye2elem) -> do
          (trivial, ty1eqElem, solution) <- go varsToInfer a1tye1elem a1tye2elem
          pure (trivial, TyEq1List ty1eqElem, solution)
        (A1TyArrow a1tye11 a1tye12, A1TyArrow a1tye21 a1tye22) -> do
          (trivial1, ty1eqDom, solution1) <- go varsToInfer a1tye11 a1tye21
          (trivial2, ty1eqCod, solution2) <- go (varsToInfer \\ Map.keysSet solution1) a1tye12 (applySolution solution1 a1tye22)
          pure (trivial1 && trivial2, TyEq1Arrow ty1eqDom ty1eqCod, Map.union solution1 solution2)
        (_, _) ->
          Left ()

mergeTypesByConditional0 :: forall trav. trav -> Bool -> Ass0Expr -> Ass0TypeExpr -> Ass0TypeExpr -> M' ConditionalMergeError trav Ass0TypeExpr
mergeTypesByConditional0 trav distributeIfUnderTensorShape a0e0 = go0
  where
    go0 :: Ass0TypeExpr -> Ass0TypeExpr -> M' ConditionalMergeError trav Ass0TypeExpr
    go0 a0tye1 a0tye2 =
      case (a0tye1, a0tye2) of
        (A0TyPrim a0tyePrim1 maybePred1, A0TyPrim a0tyePrim2 maybePred2) ->
          if a0tyePrim1 == a0tyePrim2
            then do
              maybePred <- mergeRefinementPredicates (SA0TyPrim a0tyePrim1) maybePred1 maybePred2
              pure $ A0TyPrim a0tyePrim1 maybePred
            else
              typeError trav $ CannotMerge0 a0tye1 a0tye2
        (A0TyList a0tye1' maybePred1, A0TyList a0tye2' maybePred2) -> do
          a0tye <- go0 a0tye1' a0tye2'
          maybePred <- mergeRefinementPredicates (SA0TyList (strictify a0tye1')) maybePred1 maybePred2
          pure $ A0TyList a0tye maybePred
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
          typeError trav $ CannotMerge0 a0tye1 a0tye2

    mergeRefinementPredicates :: (Maybe Ass0Expr -> StrictAss0TypeExpr) -> Maybe Ass0Expr -> Maybe Ass0Expr -> M' ConditionalMergeError trav (Maybe Ass0Expr)
    mergeRefinementPredicates sa0tyef maybePred1 maybePred2 =
      case (maybePred1, maybePred2) of
        (Nothing, Nothing) -> pure Nothing
        (Just a0ePred1, Nothing) -> makeIf (A0App a0ePred1 . A0Var) (\_ -> A0Literal (ALitBool True))
        (Nothing, Just a0ePred2) -> makeIf (\_ -> A0Literal (ALitBool True)) (A0App a0ePred2 . A0Var)
        (Just a0ePred1, Just a0ePred2) -> makeIf (A0App a0ePred1 . A0Var) (A0App a0ePred2 . A0Var)
      where
        makeIf f1 f2 = do
          ax <- AssVarStatic <$> generateFreshVar Nothing
          pure $ Just (A0Lam Nothing (ax, sa0tyef Nothing) (A0IfThenElse a0e0 (f1 ax) (f2 ax)))

    go1 :: Ass1TypeExpr -> Ass1TypeExpr -> M' ConditionalMergeError trav Ass1TypeExpr
    go1 = mergeTypesByConditional1 trav distributeIfUnderTensorShape a0e0

mergeTypesByConditional1 :: forall trav. trav -> Bool -> Ass0Expr -> Ass1TypeExpr -> Ass1TypeExpr -> M' ConditionalMergeError trav Ass1TypeExpr
mergeTypesByConditional1 trav distributeIfUnderTensorShape a0e0 = go1
  where
    go1 :: Ass1TypeExpr -> Ass1TypeExpr -> M' ConditionalMergeError trav Ass1TypeExpr
    go1 a1tye1 a1tye2 =
      case (a1tye1, a1tye2) of
        (A1TyPrim a1tyePrim1, A1TyPrim a1tyePrim2) ->
          A1TyPrim
            <$> case (a1tyePrim1, a1tyePrim2) of
              (A1TyPrimBase tyPrimBase1, A1TyPrimBase tyPrimBase2) ->
                if tyPrimBase1 == tyPrimBase2
                  then pure a1tyePrim1
                  else typeError trav $ CannotMerge1 a1tye1 a1tye2
              (A1TyTensor a0eList1, A1TyTensor a0eList2) ->
                case (a0eList1, a0eList2) of
                  -- Slight enhancement for the argument inference:
                  (A0Literal (ALitList a0es1), A0Literal (ALitList a0es2)) | distributeIfUnderTensorShape ->
                    case zipExactMay a0es1 a0es2 of
                      Nothing -> typeError trav $ CannotMerge1 a1tye1 a1tye2
                      Just zipped -> pure $ A1TyTensor (A0Literal (ALitList (map (uncurry a0branch) zipped)))
                  -- General rule:
                  (_, _) ->
                    pure $ A1TyTensor (a0branch a0eList1 a0eList2)
              _ ->
                typeError trav $ CannotMerge1 a1tye1 a1tye2
        (A1TyArrow a1tye11 a1tye12, A1TyArrow a1tye21 a1tye22) ->
          A1TyArrow <$> go1 a1tye11 a1tye21 <*> go1 a1tye12 a1tye22
        _ ->
          typeError trav $ CannotMerge1 a1tye1 a1tye2

    a0branch = A0IfThenElse a0e0

mergeResultsByConditional0 :: forall trav. trav -> Span -> Ass0Expr -> Result0 -> Result0 -> M trav Result0
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
    mergeTypes0 a0tye1 a0tye2 = do
      TypecheckConfig {distributeIfUnderTensorShape} <- askConfig
      spanInFile <- askSpanInFile loc
      mapTypeError (CannotMergeTypesByConditional0 spanInFile a0tye1 a0tye2) $
        mergeTypesByConditional0 trav distributeIfUnderTensorShape a0e0 a0tye1 a0tye2

    mergeTypes1 :: Ass1TypeExpr -> Ass1TypeExpr -> M trav Ass1TypeExpr
    mergeTypes1 a1tye1 a1tye2 = do
      TypecheckConfig {distributeIfUnderTensorShape} <- askConfig
      spanInFile <- askSpanInFile loc
      mapTypeError (CannotMergeTypesByConditional1 spanInFile a1tye1 a1tye2) $
        mergeTypesByConditional1 trav distributeIfUnderTensorShape a0e0 a1tye1 a1tye2

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

type InferenceSolution = Map AssVar (Ass0Expr, Ass0TypeExpr)

applySolution :: forall af. (HasVar StaticVar af) => InferenceSolution -> af StaticVar -> af StaticVar
applySolution solution entity =
  Map.foldrWithKey (flip subst0) entity (Map.map fst solution)

instantiateGuidedByAppContext0 :: forall trav. trav -> Span -> AppContext -> Ass0TypeExpr -> M trav Result0
instantiateGuidedByAppContext0 trav loc appCtx0 a0tye0 = do
  (result, _isubstRet) <- go Set.empty appCtx0 a0tye0
  pure result
  where
    go :: Set AssVar -> AppContext -> Ass0TypeExpr -> M trav (Result0, InferenceSolution)
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
              (a0eInferred, a0tyeInferred) <-
                case Map.lookup x solution' of
                  Just entry ->
                    pure entry
                  Nothing -> do
                    spanInFile <- askSpanInFile loc
                    typeError trav $ CannotInferOptional spanInFile x a0tye appCtx
              (cast', _solution'') <-
                makeAssertiveCast trav loc Set.empty a0tyeInferred (applySolution solution' a0tye1)
              pure (FillInferred0 (applyCast cast' a0eInferred) result', solution')
            _ -> do
              (result', solution') <- go (Set.insert x varsToInfer) appCtx a0tye2
              (a0eInferred, a0tyeInferred) <-
                case Map.lookup x solution' of
                  Just entry ->
                    pure entry
                  Nothing -> do
                    spanInFile <- askSpanInFile loc
                    typeError trav $ CannotInferOptional spanInFile x a0tye appCtx
              (cast', _solution) <-
                makeAssertiveCast trav loc Set.empty a0tyeInferred (applySolution solution' a0tye1)
              pure (InsertInferred0 (applyCast cast' a0eInferred) result', solution')
        (_, A0TyCode a1tye) -> do
          (result', solution) <- instantiateGuidedByAppContext1 trav loc varsToInfer appCtx a1tye
          result <- mapMPure (pure . A0TyCode) result'
          pure (result, solution)
        _ -> do
          spanInFile <- askSpanInFile loc
          typeError trav $ CannotInstantiateGuidedByAppContext0 spanInFile appCtx a0tye

instantiateGuidedByAppContext1 :: forall trav. trav -> Span -> Set AssVar -> AppContext -> Ass1TypeExpr -> M trav (Result1, InferenceSolution)
instantiateGuidedByAppContext1 trav loc = go
  where
    go :: Set AssVar -> AppContext -> Ass1TypeExpr -> M trav (Result1, InferenceSolution)
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

validateEmptyRetAppContext :: String -> ResultF af StaticVar -> M trav (af StaticVar)
validateEmptyRetAppContext _ (Pure v) = pure v
validateEmptyRetAppContext msg _ = bug $ "non-empty RetAppContext; " ++ msg

forceExpr0 :: trav -> TypeEnv -> Ass0TypeExpr -> Expr -> M trav Ass0Expr
forceExpr0 trav tyEnv a0tyeReq e@(Expr loc eMain) = do
  case eMain of
    Literal (LitList es) ->
      case a0tyeReq of
        A0TyList a0tyeElem _maybePred -> do
          a0es <- mapM (forceExpr0 trav tyEnv a0tyeElem) es
          pure $ A0Literal (ALitList a0es)
        _ ->
          error "TODO (error): forceExpr0, not a List type"
    _ -> do
      (result, a0e) <- typecheckExpr0 trav tyEnv [] e
      a0tye <- validateEmptyRetAppContext "forceExpr0" result
      (cast, _solution) <- makeAssertiveCast trav loc Set.empty a0tye a0tyeReq
      pure $ applyCast cast a0e

typecheckExpr0 :: trav -> TypeEnv -> AppContext -> Expr -> M trav (Result0, Ass0Expr)
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
                  -- Ad hoc optimization about `Nat`
                  pure (if n >= 0 then BuiltIn.tyNat else A0TyPrim (A0TyPrimBase ATyPrimInt) Nothing, ALitInt n)
                LitFloat r ->
                  pure (A0TyPrim (A0TyPrimBase ATyPrimFloat) Nothing, ALitFloat r)
                LitUnit ->
                  pure (A0TyPrim (A0TyPrimBase ATyPrimUnit) Nothing, ALitUnit)
                LitBool b ->
                  pure (A0TyPrim (A0TyPrimBase ATyPrimBool) Nothing, ALitBool b)
                LitString t ->
                  pure (A0TyPrim (A0TyPrimBase ATyPrimString) Nothing, ALitString t)
                LitList es ->
                  case es of
                    [] ->
                      error "TODO (error): typecheckExpr0, empty list literals"
                    eFirst : esTail -> do
                      (resultFirst, a0eFirst) <- typecheckExpr0 trav tyEnv [] eFirst
                      a0tyeFirst <- validateEmptyRetAppContext "stage-0, LitList, first" resultFirst
                      a0esTail <- mapM (forceExpr0 trav tyEnv a0tyeFirst) esTail
                      pure (A0TyList a0tyeFirst Nothing, ALitList (a0eFirst : a0esTail))
                LitVec ns -> do
                  let vec = Vector.fromList ns
                  pure (A0TyPrim (a0TyVec (Vector.length vec)) Nothing, ALitVec vec)
                LitMat nss -> do
                  mat <-
                    liftEither . mapLeft (\e -> (InvalidMatrixLiteral spanInFile e, trav)) $
                      Matrix.fromRows nss
                  pure (A0TyPrim (uncurry a0TyMat (Matrix.size mat)) Nothing, ALitMat mat)
            pure (Pure a0tye, A0Literal alit)
          _ : _ ->
            typeError trav $ CannotApplyLiteral spanInFile
      Var (ms, x) -> do
        valEntry <- findValVar trav loc ms x tyEnv
        (a0tye, builtInNameOrSv) <-
          case valEntry of
            Ass0Entry a0tye' a0metadataOrSv ->
              pure $
                (a0tye',) $
                  case a0metadataOrSv of
                    Left Ass0Metadata {ass0builtInName} -> Left ass0builtInName
                    Right svx -> Right svx
            AssPersEntry aPtye AssPersMetadata {assPbuiltInName} ->
              pure (persistentTypeTo0 aPtye, Left (unliftBuiltInName assPbuiltInName))
            Ass1Entry _ _ ->
              typeError trav $ NotAStage0Var spanInFile x
        result <- instantiateGuidedByAppContext0 trav loc appCtx a0tye
        case builtInNameOrSv of
          Left builtInName ->
            pure (result, A0BuiltInName builtInName)
          Right svX -> do
            let ax = AssVarStatic svX
            pure (result, A0Var ax)
      Lam recOpt (x1, tye1) e2 ->
        case appCtx of
          [] -> do
            svX1 <- generateFreshVar (Just x1)
            let ax1 = AssVarStatic svX1
            case recOpt of
              Nothing -> do
                a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
                (result2, a0e2) <-
                  typecheckExpr0 trav (TypeEnv.addVal x1 (Ass0Entry a0tye1 (Right svX1)) tyEnv) [] e2
                a0tye2 <- validateEmptyRetAppContext "stage-0, Lam, non-rec" result2
                let sa0tye1 = strictify a0tye1
                pure (Pure (A0TyArrow (Just ax1, a0tye1) a0tye2), A0Lam Nothing (ax1, sa0tye1) a0e2)
              Just (f, tyeRec) -> do
                svF <- generateFreshVar (Just f)
                let af = AssVarStatic svF
                a0tyeRec <- typecheckTypeExpr0 trav tyEnv tyeRec
                a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
                (result2, a0e2) <- do
                  let tyEnv' =
                        tyEnv
                          & TypeEnv.addVal x1 (Ass0Entry a0tye1 (Right svX1))
                          & TypeEnv.addVal f (Ass0Entry a0tyeRec (Right svF))
                  typecheckExpr0 trav tyEnv' [] e2
                a0tye2 <- validateEmptyRetAppContext "stage-0, Lam, rec" result2
                let a0tyeSynth = A0TyArrow (Just ax1, a0tye1) a0tye2
                (cast, _solution) <- makeAssertiveCast trav loc Set.empty a0tyeSynth a0tyeRec
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
        svX1 <- generateFreshVar (Just x1)
        let ax1 = AssVarStatic svX1
        case appCtx of
          [] -> do
            a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
            (result2, a0e2) <- do
              let tyEnv' = TypeEnv.addVal x1 (Ass0Entry a0tye1 (Right svX1)) tyEnv
              typecheckExpr0 trav tyEnv' [] e2
            a0tye2 <- validateEmptyRetAppContext "stage-1, Lam, non-rec" result2
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
      LetIn x params e1 e2 -> do
        svX <- generateFreshVar (Just x)
        let ax = AssVarStatic svX
        (a0tye1, a0e1) <- typecheckLetInBody0 trav tyEnv params e1
        (result2, a0e2) <- do
          let tyEnv' = TypeEnv.addVal x (Ass0Entry a0tye1 (Right svX)) tyEnv
          typecheckExpr0 trav tyEnv' appCtx e2
        let sa0tye1 = strictify a0tye1
        if ax `occurs0` result2
          then typeError trav $ VarOccursFreelyInAss0Type spanInFile x result2
          else pure (result2, A0LetIn (ax, sa0tye1) a0e1 a0e2)
      LetRecIn f params tyeBody eBody e2 -> do
        let tyeRec = constructFunTypeExpr0 loc params tyeBody
        a0tye1Rec <- typecheckTypeExpr0 trav tyEnv tyeRec
        (x0, tyeParam0, paramsRest) <-
          case params of
            MandatoryBinder (x0', tyeParam0') : paramsRest' -> pure (x0', tyeParam0', paramsRest')
            OptionalBinder _ : _ -> error "TODO (error): LetRecIn, optional"
            [] -> error "TODO (error): LetRecIn, empty param"
        svFInner <- generateFreshVar (Just f)
        let afInner = AssVarStatic svFInner
        svX0 <- generateFreshVar (Just x0)
        let ax0 = AssVarStatic svX0
        a0tyeParam0 <- typecheckTypeExpr0 trav tyEnv tyeParam0
        (a0tyeRestSynth, a0eRest) <- do
          let tyEnv' =
                tyEnv
                  & TypeEnv.addVal f (Ass0Entry a0tye1Rec (Right svFInner))
                  & TypeEnv.addVal x0 (Ass0Entry a0tyeParam0 (Right svX0))
          typecheckLetInBody0 trav tyEnv' paramsRest eBody
        let a0tye1Synth = A0TyArrow (Just ax0, a0tyeParam0) a0tyeRestSynth
        (cast, _solution) <- makeAssertiveCast trav loc Set.empty a0tye1Synth a0tye1Rec
        let a0e1 = applyCast cast (A0Lam (Just (afInner, strictify a0tye1Rec)) (ax0, strictify a0tyeParam0) a0eRest)
        svFOuter <- generateFreshVar (Just f)
        let afOuter = AssVarStatic svFOuter
        (result2, a0e2) <- do
          let tyEnv' = TypeEnv.addVal f (Ass0Entry a0tye1Rec (Right svFOuter)) tyEnv
          typecheckExpr0 trav tyEnv' appCtx e2
        if afOuter `occurs0` result2
          then
            typeError trav $ VarOccursFreelyInAss0Type spanInFile f result2
          else do
            pure (result2, A0LetIn (afOuter, strictify a0tye1Rec) a0e1 a0e2)
      LetTupleIn xL xR e1 e2 -> do
        (result1, a0e1) <- typecheckExpr0 trav tyEnv [] e1
        a0tye1 <- validateEmptyRetAppContext "stage-0, LetTupleIn" result1
        case a0tye1 of
          A0TyProduct a0tyeL a0tyeR -> do
            svXL <- generateFreshVar (Just xL)
            let axL = AssVarStatic svXL
            svXR <- generateFreshVar (Just xR)
            let axR = AssVarStatic svXR
            (result2, a0e2) <- do
              let tyEnv' =
                    tyEnv
                      & TypeEnv.addVal xL (Ass0Entry a0tyeL (Right svXL))
                      & TypeEnv.addVal xR (Ass0Entry a0tyeR (Right svXR))
              typecheckExpr0 trav tyEnv' appCtx e2
            pure (result2, A0LetTupleIn axL axR a0e1 a0e2)
          _ -> do
            let Expr loc1 _ = e1
            spanInFile1 <- askSpanInFile loc1
            typeError trav $ NotATupleAtStage0 spanInFile1 a0tye1
      LetOpenIn m e -> do
        case TypeEnv.findModule m tyEnv of
          Nothing ->
            typeError trav $ UnboundModule spanInFile m
          Just (ModuleEntry sigr) -> do
            let tyEnv' = TypeEnv.appendSigRecord tyEnv sigr
            typecheckExpr0 trav tyEnv' appCtx e
      Sequential e1 e2 -> do
        (result1, a0e1) <- typecheckExpr0 trav tyEnv [] e1
        a0tye1 <- validateEmptyRetAppContext "stage-0, Sequential" result1
        case a0tye1 of
          A0TyPrim (A0TyPrimBase ATyPrimUnit) _maybePred -> do
            (result2, a0e2) <- typecheckExpr0 trav tyEnv appCtx e2
            pure (result2, A0Sequential a0e1 a0e2)
          _ -> do
            let Expr loc1 _ = e1
            spanInFile1 <- askSpanInFile loc1
            typeError trav $ NotAUnitTypeForStage0 spanInFile1 a0tye1
      Tuple e1 e2 -> do
        case appCtx of
          [] -> do
            (result1, a0e1) <- typecheckExpr0 trav tyEnv [] e1
            a0tye1 <- validateEmptyRetAppContext "stage-0, Tuple, 1" result1
            (result2, a0e2) <- typecheckExpr0 trav tyEnv [] e2
            a0tye2 <- validateEmptyRetAppContext "stage-0, Tuple, 2" result2
            pure (Pure (A0TyProduct a0tye1 a0tye2), (A0Tuple a0e1 a0e2))
          _ : _ -> do
            typeError trav $ CannotApplyTuple spanInFile
      IfThenElse e0 e1 e2 -> do
        (result0, a0e0) <- typecheckExpr0 trav tyEnv [] e0
        a0tye0 <- validateEmptyRetAppContext "stage-0, IfThenElse, condition" result0
        case a0tye0 of
          A0TyPrim (A0TyPrimBase ATyPrimBool) _maybePred -> do
            (result1, a0e1) <- typecheckExpr0 trav tyEnv appCtx e1
            (result2, a0e2) <- typecheckExpr0 trav tyEnv appCtx e2
            result <- mergeResultsByConditional0 trav loc a0e0 result1 result2
            pure (result, A0IfThenElse a0e0 a0e1 a0e2)
          _ -> do
            let Expr loc0 _ = e0
            spanInFile0 <- askSpanInFile loc0
            typeError trav $ NotABoolTypeForStage0 spanInFile0 a0tye0
      As e1 tye2 ->
        case appCtx of
          [] -> do
            a0tye2 <- typecheckTypeExpr0 trav tyEnv tye2
            a0e1 <- forceExpr0 trav tyEnv a0tye2 e1
            pure (Pure a0tye2, a0e1)
          _ : _ ->
            error "TODO: stage-0, As, non-empty AppContext"
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

-- TODO (enhance): give better code position
-- TODO: refactor this; elaborate the types of the parameters before building a function type
constructFunTypeExpr0 :: Span -> [LamBinder] -> TypeExpr -> TypeExpr
constructFunTypeExpr0 loc params tyeBody =
  foldr
    ( \param tyeAcc ->
        case param of
          MandatoryBinder (x, tye) -> TypeExpr loc (TyArrow (Just x, tye) tyeAcc)
          OptionalBinder (x, tye) -> TypeExpr loc (TyOptArrow (x, tye) tyeAcc)
    )
    tyeBody
    params

-- TODO (enhance): give better code position
-- TODO: refactor this; elaborate the types of the parameters before building a function type
constructFunTypeExpr1 :: trav -> Span -> [LamBinder] -> TypeExpr -> M trav TypeExpr
constructFunTypeExpr1 trav loc params tyeBody = do
  spanInFile <- askSpanInFile loc
  foldrM
    ( \param tyeAcc ->
        case param of
          MandatoryBinder (_x, tye) -> pure $ TypeExpr loc (TyArrow (Nothing, tye) tyeAcc)
          OptionalBinder (_x, _tye) -> typeError trav $ CannotUseLamOptAtStage1 spanInFile
    )
    tyeBody
    params

typecheckLetInBody0 :: trav -> TypeEnv -> [LamBinder] -> Expr -> M trav (Ass0TypeExpr, Ass0Expr)
typecheckLetInBody0 trav tyEnv params e1 =
  case params of
    [] -> do
      (result1, a0e1) <- typecheckExpr0 trav tyEnv [] e1
      a0tye1 <- validateEmptyRetAppContext "stage-0, LetIn" result1
      pure (a0tye1, a0e1)
    MandatoryBinder (x, tye) : params' -> do
      a0tye <- typecheckTypeExpr0 trav tyEnv tye
      svX <- generateFreshVar (Just x)
      (a0tye', a0e') <- typecheckLetInBody0 trav (TypeEnv.addVal x (Ass0Entry a0tye (Right svX)) tyEnv) params' e1
      let ax = AssVarStatic svX
      pure (A0TyArrow (Just ax, a0tye) a0tye', A0Lam Nothing (ax, strictify a0tye) a0e')
    OptionalBinder (x, tye) : params' -> do
      a0tye <- typecheckTypeExpr0 trav tyEnv tye
      svX <- generateFreshVar (Just x)
      (a0tye', a0e') <- typecheckLetInBody0 trav (TypeEnv.addVal x (Ass0Entry a0tye (Right svX)) tyEnv) params' e1
      let ax = AssVarStatic svX
      pure (A0TyOptArrow (ax, a0tye) a0tye', A0Lam Nothing (ax, strictify a0tye) a0e')

typecheckExpr1 :: trav -> TypeEnv -> AppContext -> Expr -> M trav (Result1, Ass1Expr)
typecheckExpr1 trav tyEnv appCtx (Expr loc eMain) = do
  spanInFile <- askSpanInFile loc
  case eMain of
    Literal lit ->
      case appCtx of
        [] -> do
          (a1tye, alit) <-
            case lit of
              LitInt n ->
                pure (A1TyPrim (A1TyPrimBase ATyPrimInt), ALitInt n)
              LitFloat r ->
                pure (A1TyPrim (A1TyPrimBase ATyPrimFloat), ALitFloat r)
              LitUnit ->
                pure (A1TyPrim (A1TyPrimBase ATyPrimUnit), ALitUnit)
              LitBool b ->
                pure (A1TyPrim (A1TyPrimBase ATyPrimBool), ALitBool b)
              LitString t ->
                pure (A1TyPrim (A1TyPrimBase ATyPrimString), ALitString t)
              LitList es ->
                case es of
                  [] ->
                    error "TODO (error): typecheckExpr1, empty list literal"
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
                mat <-
                  liftEither . mapLeft (\e -> (InvalidMatrixLiteral spanInFile e, trav)) $
                    Matrix.fromRows nss
                pure (A1TyPrim (uncurry a1TyMat (both (A0Literal . ALitInt) (Matrix.size mat))), ALitMat mat)
          pure (Pure a1tye, A1Literal alit)
        _ : _ ->
          typeError trav $ CannotApplyLiteral spanInFile
    Var (ms, x) -> do
      valEntry <- findValVar trav loc ms x tyEnv
      (a1tye, a1builtInNameOrSv) <-
        case valEntry of
          Ass0Entry _ _ ->
            typeError trav $ NotAStage1Var spanInFile x
          AssPersEntry aPtye AssPersMetadata {assPbuiltInName} ->
            pure (persistentTypeTo1 aPtye, Left assPbuiltInName)
          Ass1Entry a1tye' a1metadataOrSv ->
            pure $
              (a1tye',) $
                case a1metadataOrSv of
                  Left Ass1Metadata {ass1builtInName} -> Left ass1builtInName
                  Right svX -> Right svX
      (result, _) <- instantiateGuidedByAppContext1 trav loc Set.empty appCtx a1tye
      case a1builtInNameOrSv of
        Left a1builtInName ->
          pure (result, A1BuiltInName a1builtInName)
        Right svX -> do
          let ax = AssVarStatic svX
          pure (result, A1Var ax)
    Lam recOpt (x1, tye1) e2 ->
      case appCtx of
        [] -> do
          svX1 <- generateFreshVar (Just x1)
          case recOpt of
            Nothing -> do
              a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
              (result2, a1e2) <-
                typecheckExpr1 trav (TypeEnv.addVal x1 (Ass1Entry a1tye1 (Right svX1)) tyEnv) [] e2
              a1tye2 <- validateEmptyRetAppContext "stage-1, Lam, non-rec" result2
              let ax1 = AssVarStatic svX1
              pure (Pure (A1TyArrow a1tye1 a1tye2), A1Lam Nothing (ax1, a1tye1) a1e2)
            Just (f, tyeRec) -> do
              svF <- generateFreshVar (Just f)
              a1tyeRec <- typecheckTypeExpr1 trav tyEnv tyeRec
              a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
              (result2, a1e2) <- do
                let tyEnv' =
                      tyEnv
                        & TypeEnv.addVal x1 (Ass1Entry a1tye1 (Right svX1))
                        & TypeEnv.addVal f (Ass1Entry a1tyeRec (Right svF))
                typecheckExpr1 trav tyEnv' [] e2
              a1tye2 <- validateEmptyRetAppContext "stage-1, Lam, rec" result2
              let ax1 = AssVarStatic svX1
              let af = AssVarStatic svF
              let a1tyeSynth = A1TyArrow a1tye1 a1tye2
              (eq, _) <- makeEquation1 trav loc Set.empty a1tyeSynth a1tyeRec
              pure (Pure a1tyeRec, applyEquationCast loc eq (A1Lam (Just (af, a1tyeRec)) (ax1, a1tye1) a1e2))
        _ : _ ->
          error "TODO (error): stage-1, Lam, non-empty AppContext"
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
    LetIn x params eBody e2 -> do
      svX <- generateFreshVar (Just x)
      (a1tye1, a1e1) <- typecheckLetInBody1 trav tyEnv params eBody
      (result2, a1e2) <-
        typecheckExpr1 trav (TypeEnv.addVal x (Ass1Entry a1tye1 (Right svX)) tyEnv) appCtx e2
      let ax = AssVarStatic svX
      if ax `occurs1` result2
        then typeError trav $ VarOccursFreelyInAss1Type spanInFile x result2
        else pure (result2, a1LetIn (ax, a1tye1) a1e1 a1e2)
    LetRecIn f params tyeBody eBody e2 -> do
      tyeRec <- constructFunTypeExpr1 trav loc params tyeBody
      a1tye1Rec <- typecheckTypeExpr1 trav tyEnv tyeRec
      (x0, tyeParam0, paramsRest) <-
        case params of
          MandatoryBinder (x0', tyeParam0') : paramsRest' -> pure (x0', tyeParam0', paramsRest')
          OptionalBinder _ : _ -> error "TODO (error): LetRecIn, optional"
          [] -> error "TODO (error): LetRecIn, empty param"
      svFInner <- generateFreshVar (Just f)
      let afInner = AssVarStatic svFInner
      svX0 <- generateFreshVar (Just x0)
      let ax0 = AssVarStatic svX0
      a1tyeParam0 <- typecheckTypeExpr1 trav tyEnv tyeParam0
      (a1tyeRestSynth, a1eRest) <- do
        let tyEnv' =
              tyEnv
                & TypeEnv.addVal f (Ass1Entry a1tye1Rec (Right svFInner))
                & TypeEnv.addVal x0 (Ass1Entry a1tyeParam0 (Right svX0))
        typecheckLetInBody1 trav tyEnv' paramsRest eBody
      let a1tye1Synth = A1TyArrow a1tyeParam0 a1tyeRestSynth
      (eq, _solution) <- makeEquation1 trav loc Set.empty a1tye1Synth a1tye1Rec
      let a1e1 = applyEquationCast loc eq (A1Lam (Just (afInner, a1tye1Rec)) (ax0, a1tyeParam0) a1eRest)
      svFOuter <- generateFreshVar (Just f)
      let afOuter = AssVarStatic svFOuter
      (result2, a1e2) <- do
        let tyEnv' = TypeEnv.addVal f (Ass1Entry a1tye1Rec (Right svFOuter)) tyEnv
        typecheckExpr1 trav tyEnv' appCtx e2
      if afOuter `occurs1` result2
        then typeError trav $ VarOccursFreelyInAss1Type spanInFile f result2
        else pure (result2, a1LetIn (afOuter, a1tye1Rec) a1e1 a1e2)
    LetTupleIn xL xR e1 e2 -> do
      (result1, a1e1) <- typecheckExpr1 trav tyEnv [] e1
      a1tye1 <- validateEmptyRetAppContext "stage-1, LetTupleIn" result1
      case a1tye1 of
        A1TyProduct a1tyeL a1tyeR -> do
          svXL <- generateFreshVar (Just xL)
          let axL = AssVarStatic svXL
          svXR <- generateFreshVar (Just xR)
          let axR = AssVarStatic svXR
          (result2, a1e2) <- do
            let tyEnv' =
                  tyEnv
                    & TypeEnv.addVal xL (Ass1Entry a1tyeL (Right svXL))
                    & TypeEnv.addVal xR (Ass1Entry a1tyeR (Right svXR))
            typecheckExpr1 trav tyEnv' appCtx e2
          pure (result2, A1LetTupleIn axL axR a1e1 a1e2)
        _ -> do
          let Expr loc1 _ = e1
          spanInFile1 <- askSpanInFile loc1
          typeError trav $ NotATupleAtStage1 spanInFile1 a1tye1
    LetOpenIn m e -> do
      case TypeEnv.findModule m tyEnv of
        Nothing ->
          typeError trav $ UnboundModule spanInFile m
        Just (ModuleEntry sigr) -> do
          let tyEnv' = TypeEnv.appendSigRecord tyEnv sigr
          typecheckExpr1 trav tyEnv' appCtx e
    Sequential e1 e2 -> do
      (result1, a1e1) <- typecheckExpr1 trav tyEnv [] e1
      a1tye1 <- validateEmptyRetAppContext "stage-1, Sequential" result1
      case a1tye1 of
        A1TyPrim (A1TyPrimBase ATyPrimUnit) -> do
          (result2, a1e2) <- typecheckExpr1 trav tyEnv appCtx e2
          pure (result2, A1Sequential a1e1 a1e2)
        _ -> do
          let Expr loc1 _ = e1
          spanInFile1 <- askSpanInFile loc1
          typeError trav $ NotAUnitTypeForStage1 spanInFile1 a1tye1
    Tuple e1 e2 -> do
      case appCtx of
        [] -> do
          (result1, a1e1) <- typecheckExpr1 trav tyEnv [] e1
          a1tye1 <- validateEmptyRetAppContext "stage-1, Tuple, 1" result1
          (result2, a1e2) <- typecheckExpr1 trav tyEnv [] e2
          a1tye2 <- validateEmptyRetAppContext "stage-1, Tuple, 2" result2
          pure (Pure (A1TyProduct a1tye1 a1tye2), A1Tuple a1e1 a1e2)
        _ : _ ->
          typeError trav $ CannotApplyTuple spanInFile
    IfThenElse e0 e1 e2 -> do
      (result0, a1e0) <- typecheckExpr1 trav tyEnv [] e0
      a1tye0 <- validateEmptyRetAppContext "stage-1, IfThenElse, condition" result0
      case a1tye0 of
        A1TyPrim (A1TyPrimBase ATyPrimBool) ->
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
    As e1 tye2 ->
      -- The following is a very ad-hoc branching.
      -- TODO: fix this with "truly" bidirectional type-checking
      case e1 of
        Expr _loc1 (Literal (LitList [])) ->
          case appCtx of
            [] -> do
              a1tye2 <- typecheckTypeExpr1 trav tyEnv tye2
              pure (Pure a1tye2, A1Literal (ALitList []))
            _ : _ ->
              typeError trav $ CannotApplyLiteral spanInFile
        _ -> do
          (result1, a1e1) <- typecheckExpr1 trav tyEnv [] e1
          a1tye1 <- validateEmptyRetAppContext "stage-1, As" result1
          a1tye2 <- typecheckTypeExpr1 trav tyEnv tye2
          (eq, _solution) <- makeEquation1 trav loc Set.empty a1tye1 a1tye2
          pure (Pure a1tye2, applyEquationCast loc eq a1e1)
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

typecheckLetInBody1 :: trav -> TypeEnv -> [LamBinder] -> Expr -> M trav (Ass1TypeExpr, Ass1Expr)
typecheckLetInBody1 trav tyEnv params e1 =
  case params of
    [] -> do
      (result1, a1e1) <- typecheckExpr1 trav tyEnv [] e1
      a1tye1 <- validateEmptyRetAppContext "stage-0, LetIn" result1
      pure (a1tye1, a1e1)
    MandatoryBinder (x, tye) : params' -> do
      a1tye <- typecheckTypeExpr1 trav tyEnv tye
      svX <- generateFreshVar (Just x)
      (a1tye', a1e') <- typecheckLetInBody1 trav (TypeEnv.addVal x (Ass1Entry a1tye (Right svX)) tyEnv) params' e1
      let ax = AssVarStatic svX
      pure (A1TyArrow a1tye a1tye', A1Lam Nothing (ax, a1tye) a1e')
    OptionalBinder (_x, tye) : _params' -> do
      let TypeExpr loc _ = tye -- TODO (enhance): give a better code position
      spanInFile <- askSpanInFile loc
      typeError trav $ CannotUseLamOptAtStage1 spanInFile

mapMPure :: (af StaticVar -> M trav (bf StaticVar)) -> ResultF af StaticVar -> M trav (ResultF bf StaticVar)
mapMPure f = go
  where
    go (Pure v) = Pure <$> f v
    go (Cast0 cast a0tye r) = Cast0 cast a0tye <$> go r
    go (Cast1 eq a1tye r) = Cast1 eq a1tye <$> go r
    go (CastGiven0 a0e a0tye r) = CastGiven0 a0e a0tye <$> go r
    go (FillInferred0 a0e r) = FillInferred0 a0e <$> go r
    go (InsertInferred0 a0e r) = InsertInferred0 a0e <$> go r

validateIntLiteral :: trav -> (IntermediateArgForAss0Type, Span) -> M trav Int
validateIntLiteral trav = \case
  (IA0ExprArg (A0Literal (ALitInt n)), _) ->
    pure n
  (IA0ExprArg a0e, loc) -> do
    spanInFile <- askSpanInFile loc
    typeError trav $ NotAnIntLitArgAtStage0 spanInFile a0e
  (IA0TypeArg a0tye, loc) -> do
    spanInFile <- askSpanInFile loc
    typeError trav $ NotAValueArgAtStage0 spanInFile a0tye

validateIntListLiteral :: trav -> (IntermediateArgForAss0Type, Span) -> M trav [Int]
validateIntListLiteral trav = \case
  (IA0ExprArg a0e@(A0Literal (ALitList a0es)), loc) -> do
    spanInFile <- askSpanInFile loc
    mapM
      ( \case
          A0Literal (ALitInt n) -> pure n
          _ -> typeError trav $ NotAnIntListLitArgAtStage0 spanInFile a0e
      )
      a0es
  (IA0ExprArg a0e, loc) -> do
    spanInFile <- askSpanInFile loc
    typeError trav $ NotAnIntListLitArgAtStage0 spanInFile a0e
  (IA0TypeArg a0tye, loc) -> do
    spanInFile <- askSpanInFile loc
    typeError trav $ NotAValueArgAtStage0 spanInFile a0tye

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
        ("Nat", []) ->
          pure BuiltIn.tyNat
        (_, []) ->
          case validatePrimBaseType tyName of
            Just tyPrimBase -> pure $ A0TyPrim (A0TyPrimBase tyPrimBase) Nothing
            Nothing -> typeError trav $ UnknownTypeOrInvalidArityAtStage0 spanInFile tyName 0
        ("List", [arg]) -> do
          case arg of
            (IA0TypeArg a0tye, _) -> pure $ A0TyList a0tye Nothing
            (IA0ExprArg _a0e, _loc) -> error "TODO (error): typecheckTypeExpr0, TyName, List, IA0ExprArg"
        ("Vec", [arg]) -> do
          n <- validateIntLiteral trav arg
          pure $ A0TyPrim (a0TyVec n) Nothing
        ("Mat", [arg1, arg2]) -> do
          m <- validateIntLiteral trav arg1
          n <- validateIntLiteral trav arg2
          pure $ A0TyPrim (a0TyMat m n) Nothing
        ("Tensor", [arg]) -> do
          ns <- validateIntListLiteral trav arg
          pure $ A0TyPrim (A0TyTensor ns) Nothing
        _ -> typeError trav $ UnknownTypeOrInvalidArityAtStage0 spanInFile tyName (List.length results)
    TyVar tyvar -> do
      TypeVarEntry atyvar <- findTypeVar trav loc tyvar tyEnv
      pure $ A0TyVar atyvar
    TyArrow (xOpt, tye1) tye2 -> do
      a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
      (tyEnv', svXOpt) <-
        case xOpt of
          Just x -> do
            svX <- generateFreshVar (Just x)
            pure (TypeEnv.addVal x (Ass0Entry a0tye1 (Right svX)) tyEnv, Just svX)
          Nothing ->
            pure (tyEnv, Nothing)
      a0tye2 <- typecheckTypeExpr0 trav tyEnv' tye2
      let axOpt = AssVarStatic <$> svXOpt
      pure $ A0TyArrow (axOpt, a0tye1) a0tye2
    TyCode tye1 -> do
      a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
      pure $ A0TyCode a1tye1
    TyOptArrow (x, tye1) tye2 -> do
      a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
      svX <- generateFreshVar (Just x)
      a0tye2 <- do
        let tyEnv' = TypeEnv.addVal x (Ass0Entry a0tye1 (Right svX)) tyEnv
        typecheckTypeExpr0 trav tyEnv' tye2
      let ax = AssVarStatic svX
      pure $ A0TyOptArrow (ax, a0tye1) a0tye2
    TyRefinement x tye1 e2 -> do
      a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
      svX <- generateFreshVar (Just x)
      (result2, a0e2) <- typecheckExpr0 trav (TypeEnv.addVal x (Ass0Entry a0tye1 (Right svX)) tyEnv) [] e2
      a0tye2 <- validateEmptyRetAppContext "TyRefinement" result2
      case a0tye2 of
        A0TyPrim (A0TyPrimBase ATyPrimBool) _maybePredForBool -> do
          let ax = AssVarStatic svX
          case a0tye1 of
            A0TyPrim a0tyPrim Nothing -> do
              pure $
                A0TyPrim a0tyPrim . Just $
                  A0Lam Nothing (ax, strictify a0tye1) a0e2
            A0TyPrim a0tyPrim (Just a0ePredForBase) -> do
              pure $
                A0TyPrim a0tyPrim . Just $
                  A0Lam Nothing (ax, strictify a0tye1) $
                    A0App (A0App ass0exprAnd (A0App a0ePredForBase (A0Var ax))) a0e2
            A0TyList a0tyeElem Nothing -> do
              pure $
                A0TyList a0tyeElem . Just $
                  A0Lam Nothing (ax, strictify a0tye1) a0e2
            A0TyList a0tyeElem (Just a0ePredForBase) -> do
              pure $
                A0TyList a0tyeElem . Just $
                  A0Lam Nothing (ax, strictify a0tye1) $
                    A0App (A0App ass0exprAnd (A0App a0ePredForBase (A0Var ax))) a0e2
            _ -> do
              let TypeExpr loc1 _ = tye1
              spanInFile1 <- askSpanInFile loc1
              typeError trav $ InvalidTypeForRefinement spanInFile1 a0tye1
        _ -> do
          let Expr loc2 _ = e2
          spanInFile2 <- askSpanInFile loc2
          typeError trav $ NotABoolTypeForStage0 spanInFile2 a0tye2
    TyProduct tye1 tye2 -> do
      a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
      a0tye2 <- typecheckTypeExpr0 trav tyEnv tye2
      pure $ A0TyProduct a0tye1 a0tye2

ass0exprAnd :: Ass0Expr
ass0exprAnd = A0BuiltInName (BuiltInArity2 BIAnd)

ass0exprListMap :: Ass0Expr
ass0exprListMap = A0BuiltInName (BuiltInArity2 BIListMap)

typecheckTypeExpr1 :: trav -> TypeEnv -> TypeExpr -> M trav Ass1TypeExpr
typecheckTypeExpr1 trav tyEnv (TypeExpr loc tyeMain) = do
  spanInFile <- askSpanInFile loc
  case tyeMain of
    TyName tyName args -> do
      mapM_
        ( \case
            ExprArgPersistent _ ->
              pure ()
            ExprArgNormal (Expr loc' _) -> do
              spanInFile' <- askSpanInFile loc'
              typeError trav $ CannotUseNormalArgAtStage1 spanInFile'
            TypeArg _ ->
              pure ()
        )
        args
      case (tyName, args) of
        (_, []) ->
          case validatePrimBaseType tyName of
            Just tyPrimBase -> pure $ A1TyPrim (A1TyPrimBase tyPrimBase)
            Nothing -> typeError trav $ UnknownTypeOrInvalidArityAtStage1 spanInFile tyName 0
        ("List", [TypeArg tye]) -> do
          a1tye <- typecheckTypeExpr1 trav tyEnv tye
          pure $ A1TyList a1tye
        ("Vec", [ExprArgPersistent e]) -> do
          a0e <- forceExpr0 trav tyEnv BuiltIn.tyNat e
          pure $ A1TyPrim (a1TyVec a0e)
        ("Mat", [ExprArgPersistent e1, ExprArgPersistent e2]) -> do
          a0e1 <- forceExpr0 trav tyEnv BuiltIn.tyNat e1
          a0e2 <- forceExpr0 trav tyEnv BuiltIn.tyNat e2
          pure $ A1TyPrim (a1TyMat a0e1 a0e2)
        ("Tensor", [ExprArgPersistent e]) -> do
          a0eList <- forceExpr0 trav tyEnv (A0TyList BuiltIn.tyNat Nothing) e
          pure $ A1TyPrim (A1TyTensor a0eList)
        _ -> typeError trav $ UnknownTypeOrInvalidArityAtStage1 spanInFile tyName (List.length args)
    TyVar _tyvar ->
      typeError trav $ CannotUseTypeVarAtStage1 spanInFile
    TyArrow (xOpt, tye1) tye2 -> do
      a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
      () <-
        case xOpt of
          Nothing -> pure ()
          Just x -> typeError trav $ FunctionTypeCannotBeDependentAtStage1 spanInFile x
      a1tye2 <- typecheckTypeExpr1 trav tyEnv tye2
      pure $ A1TyArrow a1tye1 a1tye2
    TyOptArrow _ _ ->
      typeError trav $ CannotUseOptArrowTypeAtStage1 spanInFile
    TyCode _ -> do
      typeError trav $ CannotUseCodeTypeAtStage1 spanInFile
    TyRefinement _ _ _ -> do
      typeError trav $ CannotUseRefinementTypeAtStage1 spanInFile
    TyProduct tye1 tye2 -> do
      a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
      a1tye2 <- typecheckTypeExpr1 trav tyEnv tye2
      pure $ A1TyProduct a1tye1 a1tye2

validatePersistentType :: trav -> Span -> Ass0TypeExpr -> M trav AssPersTypeExpr
validatePersistentType trav loc a0tye =
  case go a0tye of
    Just aPtye ->
      pure aPtye
    Nothing -> do
      spanInFile <- askSpanInFile loc
      typeError trav $ InvalidPersistentType spanInFile a0tye
  where
    go = \case
      A0TyPrim a0tyPrim maybePred ->
        case maybePred of
          Nothing -> pure $ APersTyPrim a0tyPrim
          Just _ -> Nothing
      A0TyVar atyvar ->
        pure $ APersTyVar atyvar
      A0TyList a0tye' maybePred ->
        case maybePred of
          Nothing -> APersTyList <$> go a0tye'
          Just _ -> Nothing
      A0TyProduct a0tye1 a0tye2 ->
        APersTyProduct <$> go a0tye1 <*> go a0tye2
      A0TyArrow (Nothing, a0tye1) a0tye2 -> do
        aPtye1 <- go a0tye1
        aPtye2 <- go a0tye2
        pure $ APersTyArrow aPtye1 aPtye2
      A0TyArrow (Just _x, _a0tye1) _a0tye2 -> do
        Nothing
      A0TyOptArrow (_x, _a0tye1) _a0tye2 -> do
        Nothing
      A0TyCode _ ->
        Nothing

extractFromExternal :: ExternalField -> External -> Maybe Text
extractFromExternal field0 =
  List.firstJust (\(field, s) -> if field == field0 then Just s else Nothing)

typecheckBind :: trav -> TypeEnv -> Bind -> M trav (SigRecord, [AssBind])
typecheckBind trav tyEnv (Bind loc bindMain) =
  case bindMain of
    BindVal stage x (BindValExternal tyvars tye ext) -> do
      extName <-
        case extractFromExternal "builtin" ext of
          Just s ->
            pure s
          Nothing -> do
            spanInFile <- askSpanInFile loc
            typeError trav $ NoBuiltInNameInExternal spanInFile
      let surfaceName = extractFromExternal "surface" ext
      tyEnv' <-
        foldM
          ( \tyEnv0 tyvar -> do
              atyvar <- generateFreshTypeVar tyvar
              pure $ TypeEnv.addTypeVar tyvar (TypeVarEntry atyvar) tyEnv0
          )
          tyEnv
          tyvars
      case stage of
        Stage0 -> do
          a0tye <- typecheckTypeExpr0 trav tyEnv' tye
          ass0builtInName <-
            case validateExternalName0 extName of
              Just a0builtInName' ->
                pure a0builtInName'
              Nothing -> do
                spanInFile <- askSpanInFile loc
                typeError trav $ UnknownExternalName spanInFile extName
          let a0metadata = Ass0Metadata {ass0builtInName, ass0surfaceName = surfaceName}
          pure (SigRecord.singletonVal x (Ass0Entry a0tye (Left a0metadata)), [])
        Stage1 -> do
          ass1builtInName <-
            case validateExternalName1 extName of
              Just ass1builtInName' ->
                pure ass1builtInName'
              Nothing -> do
                spanInFile <- askSpanInFile loc
                typeError trav $ UnknownExternalName spanInFile extName
          a1tye <- typecheckTypeExpr1 trav tyEnv' tye
          let a1metadata = Ass1Metadata {ass1builtInName, ass1surfaceName = surfaceName}
          pure (SigRecord.singletonVal x (Ass1Entry a1tye (Left a1metadata)), [])
        StagePers -> do
          a0tye <- typecheckTypeExpr0 trav tyEnv' tye
          aPtye <- validatePersistentType trav loc a0tye
          assPbuiltInName <-
            case validateExternalName1 extName of
              Just a1builtInName' ->
                pure a1builtInName'
              Nothing -> do
                spanInFile <- askSpanInFile loc
                typeError trav $ UnknownExternalName spanInFile extName
          let aPmetadata = AssPersMetadata {assPbuiltInName, assPsurfaceName = surfaceName}
          pure (SigRecord.singletonVal x (AssPersEntry aPtye aPmetadata), [])
    BindVal stage x (BindValNormal e) -> do
      svX <- generateFreshVar (Just x)
      let ax = AssVarStatic svX
      case stage of
        Stage0 -> do
          (result, a0e) <- typecheckExpr0 trav tyEnv [] e
          a0tye <- validateEmptyRetAppContext "BindVal, Stage0" result
          let sa0tye = strictify a0tye
          pure (SigRecord.singletonVal x (Ass0Entry a0tye (Right svX)), [ABind0 (ax, sa0tye) a0e])
        Stage1 -> do
          (result, a1e) <- typecheckExpr1 trav tyEnv [] e
          a1tye <- validateEmptyRetAppContext "BindVal, Stage1" result
          pure (SigRecord.singletonVal x (Ass1Entry a1tye (Right svX)), [ABind1 (ax, a1tye) a1e])
        StagePers ->
          error "TODO: typecheckBind, BindValNormal, StagePers"
    BindModule m binds -> do
      (_, sigr, abinds) <- typecheckBinds trav tyEnv binds
      pure (SigRecord.singletonModule m (ModuleEntry sigr), abinds)

typecheckBinds :: trav -> TypeEnv -> [Bind] -> M trav (TypeEnv, SigRecord, [AssBind])
typecheckBinds trav tyEnv =
  foldM
    ( \(tyEnv', sigr', abinds') bind@(Bind loc _) -> do
        (sigr, abinds) <- typecheckBind trav tyEnv' bind
        case SigRecord.intersection sigr' sigr of
          ([], []) ->
            pure (TypeEnv.appendSigRecord tyEnv' sigr, SigRecord.union sigr' sigr, abinds' ++ abinds)
          (x : _, _) -> do
            spanInFile <- askSpanInFile loc
            typeError trav $ BindingOverwritten spanInFile x
          (_, m : _) -> do
            spanInFile <- askSpanInFile loc
            typeError trav $ BindingOverwritten spanInFile m
    )
    (tyEnv, SigRecord.empty, [])

run :: M trav a -> TypecheckConfig -> TypecheckState -> (Either (TypeError, trav) a, TypecheckState)
run (M' checker) config st = runReader (runStateT checker st) config

-- runStateT :: StateT s m b -> s -> m (b, s)
-- s = TypecheckState
-- m = Reader TypecheckConfig
-- b = Either (TypeError, trav) a
