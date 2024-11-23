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
  pure $ A0Lam Nothing (x, a0tye) (A0Var x)

applyCast :: Maybe Ass0Expr -> Ass0Expr -> Ass0Expr
applyCast = maybe id A0App

applyEquationCast :: Span -> Maybe Type1Equation -> Ass1Expr -> Ass1Expr
applyEquationCast loc ty1eqOpt a1e =
  case ty1eqOpt of
    Nothing -> a1e
    Just ty1eq -> A1Escape (A0App (A0TyEqAssert loc ty1eq) (A0Bracket a1e))

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
            (A0TyVec n1, A0TyVec n2) | n1 == n2 -> nothingOrIdentityLam (A0TyPrim (A0TyVec n1))
            (A0TyMat m1 n1, A0TyMat m2 n2) | m1 == m2 && n1 == n2 -> nothingOrIdentityLam (A0TyPrim (A0TyMat m1 n1))
            _ -> typeError trav $ TypeContradictionAtStage0 spanInFile a0tye1 a0tye2
        (A0TyArrow (x1opt, a0tye11) a0tye12, A0TyArrow (x2opt, a0tye21) a0tye22) -> do
          (a0eDomCastOpt, solutionDom) <- go varsToInfer a0tye11 a0tye21
          (x, a0tye22') <-
            case (x1opt, x2opt) of
              (Nothing, Nothing) -> do
                x0 <- generateFreshVar
                pure (x0, a0tye22)
              (Just x1, Nothing) ->
                pure (x1, a0tye22)
              (Nothing, Just x2) ->
                pure (x2, a0tye22)
              (Just x1, Just x2) ->
                pure (x1, subst0 (A0Var x1) x2 a0tye22)
          (a0eCodCastOpt, solutionCod) <-
            go (varsToInfer \\ Map.keysSet solutionDom) a0tye12 (applySolution solutionDom a0tye22')
          f <- generateFreshVar
          x' <- generateFreshVar
          a0eCastOpt <-
            case (a0eDomCastOpt, a0eCodCastOpt) of
              (Nothing, Nothing) ->
                pure Nothing
              _ -> do
                let fDom = applyCast a0eDomCastOpt
                let fCod = applyCast a0eCodCastOpt
                pure $
                  Just $
                    A0Lam Nothing (f, a0tye1) $
                      A0Lam Nothing (x, a0tye21) $
                        A0App (A0Lam Nothing (x', a0tye11) (fCod (A0App (A0Var f) (A0Var x')))) (fDom (A0Var x))
          pure (a0eCastOpt, Map.union solutionDom solutionCod)
        (A0TyOptArrow (x1, a0tye11) a0tye12, A0TyOptArrow (x2, a0tye21) a0tye22) -> do
          (a0eDomCastOpt, solutionDom) <- go varsToInfer a0tye11 a0tye21
          let (x, a0tye22') = (x1, subst0 (A0Var x1) x2 a0tye22)
          (a0eCodCastOpt, solutionCod) <-
            go (varsToInfer \\ Map.keysSet solutionDom) a0tye12 (applySolution solutionDom a0tye22')
          f <- generateFreshVar
          x' <- generateFreshVar
          a0eCastOpt <-
            case (a0eDomCastOpt, a0eCodCastOpt) of
              (Nothing, Nothing) ->
                pure Nothing
              _ -> do
                let fDom = applyCast a0eDomCastOpt
                let fCod = applyCast a0eCodCastOpt
                pure $
                  Just $
                    A0Lam Nothing (f, a0tye1) $
                      A0Lam Nothing (x, a0tye21) $
                        A0App (A0Lam Nothing (x', a0tye11) (fCod (A0App (A0Var f) (A0Var x')))) (fDom (A0Var x))
          pure (a0eCastOpt, Map.union solutionDom solutionCod)
        (A0TyCode a1tye1, A0TyCode a1tye2) -> do
          (ty1eqOpt, solution) <- makeEquation1 trav loc varsToInfer a1tye1 a1tye2
          pure (A0TyEqAssert loc <$> ty1eqOpt, solution)
        _ ->
          typeError trav $ TypeContradictionAtStage0 spanInFile a0tye1 a0tye2

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
    go :: Set AssVar -> Ass1TypeExpr -> Ass1TypeExpr -> Either () (Bool, Type1Equation, InferenceSolution)
    go varsToInfer a1tye1 a1tye2 =
      case (a1tye1, a1tye2) of
        (A1TyPrim a1tyPrim1, A1TyPrim a1tyPrim2) ->
          case (a1tyPrim1, a1tyPrim2) of
            (A1TyInt, A1TyInt) ->
              pure (True, TyEq1Prim TyEq1Int, Map.empty)
            (A1TyBool, A1TyBool) ->
              pure (True, TyEq1Prim TyEq1Bool, Map.empty)
            (A1TyVec a0e1, A1TyVec a0e2) -> do
              let (trivial, a0e2', solution) =
                    case a0e2 of
                      A0Var x
                        | x `elem` varsToInfer ->
                            ( True,
                              a0e1,
                              Map.singleton x a0e1
                            )
                      _ ->
                        ( alphaEquivalent a0e1 a0e2,
                          a0e2,
                          Map.empty
                        )
              pure (trivial, TyEq1Prim (TyEq1Vec a0e1 a0e2'), solution)
            (A1TyMat a0e11 a0e12, A1TyMat a0e21 a0e22) -> do
              let (trivial, a0e21', a0e22', solution) =
                    case (a0e21, a0e22) of
                      (A0Var x1, A0Var x2)
                        | x1 `elem` varsToInfer && x2 `elem` varsToInfer ->
                            ( True,
                              a0e11,
                              a0e12,
                              Map.empty & Map.insert x1 a0e11 & Map.insert x2 a0e12
                            )
                      (A0Var x1, _)
                        | x1 `elem` varsToInfer ->
                            ( alphaEquivalent a0e12 a0e22,
                              a0e11,
                              a0e22,
                              Map.singleton x1 a0e11
                            )
                      (_, A0Var x2)
                        | x2 `elem` varsToInfer ->
                            ( alphaEquivalent a0e11 a0e21,
                              a0e21,
                              a0e12,
                              Map.singleton x2 a0e12
                            )
                      (_, _) ->
                        ( alphaEquivalent a0e11 a0e21 && alphaEquivalent a0e12 a0e22,
                          a0e21,
                          a0e22,
                          Map.empty
                        )
              pure (trivial, TyEq1Prim (TyEq1Mat a0e11 a0e12 a0e21' a0e22'), solution)
            (_, _) ->
              Left ()
        (A1TyArrow a1tye11 a1tye12, A1TyArrow a1tye21 a1tye22) -> do
          (trivial1, ty1eqDom, solution1) <- go varsToInfer a1tye11 a1tye21
          (trivial2, ty1eqCod, solution2) <- go (varsToInfer \\ Map.keysSet solution1) a1tye12 (applySolution solution1 a1tye22)
          pure (trivial1 && trivial2, TyEq1Arrow ty1eqDom ty1eqCod, Map.union solution1 solution2)
        (_, _) ->
          Left ()

unifyTypesByConditional :: trav -> Span -> Ass0Expr -> Ass0TypeExpr -> Ass0TypeExpr -> M trav Ass0TypeExpr
unifyTypesByConditional trav loc a0e0 a0tye1' a0tye2' =
  case go0 a0tye1' a0tye2' of
    Left condErr -> do
      spanInFile <- askSpanInFile loc
      typeError trav $ CannotUnifyTypesByConditional spanInFile a0tye1' a0tye2' condErr
    Right a0tye ->
      pure a0tye
  where
    go0 :: Ass0TypeExpr -> Ass0TypeExpr -> Either ConditionalUnificationError Ass0TypeExpr
    go0 a0tye1 a0tye2 =
      case (a0tye1, a0tye2) of
        (A0TyPrim a0tyePrim1, A0TyPrim a0tyePrim2) ->
          if a0tyePrim1 == a0tyePrim2
            then pure a0tye1
            else Left $ CannotUnify0 a0tye1 a0tye2
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
          Left $ CannotUnify0 a0tye1 a0tye2

    go1 :: Ass1TypeExpr -> Ass1TypeExpr -> Either ConditionalUnificationError Ass1TypeExpr
    go1 a1tye1 a1tye2 =
      case (a1tye1, a1tye2) of
        (A1TyPrim a1tyePrim1, A1TyPrim a1tyePrim2) ->
          A1TyPrim
            <$> case (a1tyePrim1, a1tyePrim2) of
              (A1TyInt, A1TyInt) ->
                pure A1TyInt
              (A1TyBool, A1TyBool) ->
                pure A1TyBool
              (A1TyVec a0e1, A1TyVec a0e2) ->
                pure $ A1TyVec (A0IfThenElse a0e0 a0e1 a0e2)
              (A1TyMat a0e11 a0e12, A1TyMat a0e21 a0e22) ->
                pure $ A1TyMat (A0IfThenElse a0e0 a0e11 a0e21) (A0IfThenElse a0e0 a0e12 a0e22)
              _ ->
                Left $ CannotUnify1 a1tye1 a1tye2
        (A1TyArrow a1tye11 a1tye12, A1TyArrow a1tye21 a1tye22) ->
          A1TyArrow <$> go1 a1tye11 a1tye21 <*> go1 a1tye12 a1tye22
        _ ->
          Left $ CannotUnify1 a1tye1 a1tye2

unifyRetAppContextsByConditional :: trav -> Span -> Ass0Expr -> RetAppContext -> RetAppContext -> M trav RetAppContext
unifyRetAppContextsByConditional _trav _loc _a0e0 retAppCtx1 _retAppCtx2 =
  pure retAppCtx1 -- TODO: fix this
  --  go
  --  where
  --    go retAppCtx1 retAppCtx2 =
  --      case (retAppCtx1, retAppCtx2) of
  --        ([], []) -> []
  --        (RetCast0 (Just a0e1) : retAppCtx1', RetCast0 (Just a0e2) : retAppCtx2') ->
  --          RetCast0 (IfThenElse a0e0 a0e1 a0e2) : go retAppCtx1' retAppCtx2'

type InferenceSolution = Map AssVar Ass0Expr

applySolution :: forall a. (HasVar a) => InferenceSolution -> a -> a
applySolution solution entity =
  Map.foldrWithKey (flip subst0) entity solution

instantiateGuidedByAppContext0 :: forall trav. trav -> Span -> AppContext -> Ass0TypeExpr -> M trav (Ass0TypeExpr, RetAppContext)
instantiateGuidedByAppContext0 trav loc appCtx0 a0tye0 = do
  (a0e, retAppCtx, _isubstRet) <- go Set.empty appCtx0 a0tye0
  pure (a0e, retAppCtx)
  where
    go :: Set AssVar -> AppContext -> Ass0TypeExpr -> M trav (Ass0TypeExpr, RetAppContext, InferenceSolution)
    go varsToInfer appCtx a0tye =
      case (appCtx, a0tye) of
        ([], _) ->
          pure (a0tye, [], Map.empty)
        (AppArg0 a0e1' a0tye1' : appCtx', A0TyArrow (xOpt, a0tye1) a0tye2) -> do
          (a0eCastOpt, solution1) <- makeAssertiveCast trav loc varsToInfer a0tye1' a0tye1
          let varsToInfer' = varsToInfer \\ Map.keysSet solution1
          let a0tye2s = applySolution solution1 a0tye2
          (a0tye2', retAppCtx', solution') <-
            case xOpt of
              Nothing -> go varsToInfer' appCtx' a0tye2s
              Just x -> go varsToInfer' appCtx' (subst0 a0e1' x a0tye2s)
          let solution = Map.union solution1 solution'
          let a0tye1s = applySolution solution a0tye1
          pure (A0TyArrow (xOpt, a0tye1s) a0tye2', RetCast0 a0eCastOpt : retAppCtx', solution)
        (appCtxEntry : appCtx', A0TyOptArrow (x, a0tye1) a0tye2) ->
          case appCtxEntry of
            AppArgOpt0 a0e1' a0tye1' -> do
              (a0eCastOpt, solution1) <- makeAssertiveCast trav loc varsToInfer a0tye1' a0tye1
              let varsToInfer' = varsToInfer \\ Map.keysSet solution1
              let a0tye2s = applySolution solution1 a0tye2
              (a0tye2', retAppCtx', solution') <- go varsToInfer' appCtx' (subst0 a0e1' x a0tye2s)
              let solution = Map.union solution1 solution'
              let a0tye1s = applySolution solution a0tye1
              pure (A0TyOptArrow (x, a0tye1s) a0tye2', RetCast0 a0eCastOpt : retAppCtx', solution)
            _ -> do
              (a0tye2', retAppCtx', solution') <- go (Set.insert x varsToInfer) appCtx a0tye2
              a0eInferred <-
                case Map.lookup x solution' of
                  Just a0eInferred' ->
                    pure a0eInferred'
                  Nothing -> do
                    spanInFile <- askSpanInFile loc
                    typeError trav $ CannotInferOptional spanInFile x
              pure (A0TyOptArrow (x, a0tye1) a0tye2', RetInferred0 a0eInferred : retAppCtx', solution')
        (_, A0TyCode a1tye) -> do
          (a1tye', retAppCtx, solution) <- instantiateGuidedByAppContext1 trav loc varsToInfer appCtx a1tye
          pure (A0TyCode a1tye', retAppCtx, solution)
        _ -> do
          spanInFile <- askSpanInFile loc
          typeError trav $ CannotInstantiateGuidedByAppContext0 spanInFile appCtx a0tye

instantiateGuidedByAppContext1 :: forall trav. trav -> Span -> Set AssVar -> AppContext -> Ass1TypeExpr -> M trav (Ass1TypeExpr, RetAppContext, InferenceSolution)
instantiateGuidedByAppContext1 trav loc = go
  where
    go :: Set AssVar -> AppContext -> Ass1TypeExpr -> M trav (Ass1TypeExpr, RetAppContext, InferenceSolution)
    go varsToInfer appCtx a1tye =
      case (appCtx, a1tye) of
        ([], _) ->
          pure (a1tye, [], Map.empty)
        (AppArg1 a1tye1' : appCtx', A1TyArrow a1tye1 a1tye2) -> do
          (ty1eq, solution1) <- makeEquation1 trav loc varsToInfer a1tye1' a1tye1
          (a1tye2', retAppCtx', solution') <-
            go (varsToInfer \\ Map.keysSet solution1) appCtx' (applySolution solution1 a1tye2)
          let solution = Map.union solution1 solution'
          let a1tye1s = applySolution solution a1tye1
          pure (A1TyArrow a1tye1s a1tye2', RetCast1 ty1eq : retAppCtx', solution)
        _ -> do
          spanInFile <- askSpanInFile loc
          typeError trav $ CannotInstantiateGuidedByAppContext1 spanInFile appCtx a1tye

validateEmptyRetAppContext :: String -> RetAppContext -> M trav ()
validateEmptyRetAppContext _ [] = pure ()
validateEmptyRetAppContext msg (_ : _) = error $ "bug: non-empty RetAppContext; " ++ msg

typecheckExpr0 :: trav -> TypeEnv -> AppContext -> Expr -> M trav (Ass0TypeExpr, Ass0Expr, RetAppContext)
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
                LitVec ns -> do
                  let vec = Vector.fromList ns
                  pure (A0TyPrim (A0TyVec (Vector.length vec)), ALitVec vec)
                LitMat nss -> do
                  mat <- lift . mapLeft (\e -> (InvalidMatrixLiteral spanInFile e, trav)) $ Matrix.fromRows nss
                  pure (A0TyPrim (uncurry A0TyMat (Matrix.size mat)), ALitMat mat)
            pure (a0tye, A0Literal alit, [])
          _ : _ ->
            typeError trav $ CannotApplyLiteral spanInFile
      Var x -> do
        entry <- findVar trav loc x tyEnv
        let ax = AssVar x
        case entry of
          TypeEnv.Ass0Entry a0tye -> do
            (a0tyeInst, retCtx) <- instantiateGuidedByAppContext0 trav loc appCtx a0tye
            pure (a0tyeInst, A0Var ax, retCtx)
          TypeEnv.Ass1Entry _ ->
            typeError trav $ NotAStage0Var spanInFile x
      Lam recOpt (x1, tye1) e2 ->
        case appCtx of
          [] ->
            case recOpt of
              Nothing -> do
                a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
                (a0tye2, a0e2, retAppCtx) <-
                  typecheckExpr0 trav (TypeEnv.addVar x1 (TypeEnv.Ass0Entry a0tye1) tyEnv) [] e2
                validateEmptyRetAppContext "stage-0, Lam, non-rec" retAppCtx
                let ax1 = AssVar x1
                pure (A0TyArrow (Just ax1, a0tye1) a0tye2, A0Lam Nothing (ax1, a0tye1) a0e2, [])
              Just (f, tyeRec) -> do
                a0tyeRec <- typecheckTypeExpr0 trav tyEnv tyeRec
                a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
                (a0tye2, a0e2, retAppCtx) <- do
                  let tyEnv' =
                        tyEnv
                          & TypeEnv.addVar x1 (TypeEnv.Ass0Entry a0tye1)
                          & TypeEnv.addVar f (TypeEnv.Ass0Entry a0tyeRec)
                  typecheckExpr0 trav tyEnv' [] e2
                validateEmptyRetAppContext "stage-0, Lam, rec" retAppCtx
                let ax1 = AssVar x1
                let af = AssVar f
                let a0tyeSynth = A0TyArrow (Just ax1, a0tye1) a0tye2
                (a0eCastOpt, _) <- makeAssertiveCast trav loc Set.empty a0tyeSynth a0tyeRec
                pure (a0tyeRec, applyCast a0eCastOpt (A0Lam (Just (af, a0tyeRec)) (ax1, a0tye1) a0e2), [])
          _ : _ ->
            error "TODO: stage-0, Lam, non-empty AppContext"
      App e1 e2 -> do
        (a0tye2, a0e2, retAppCtx2) <- typecheckExpr0 trav tyEnv [] e2
        validateEmptyRetAppContext "stage-0, App, arg" retAppCtx2
        (a0tye1, a0e1, retAppCtx1) <- typecheckExpr0 trav tyEnv (AppArg0 a0e2 a0tye2 : appCtx) e1
        case retAppCtx1 of
          RetCast0 a0eCastOpt : retAppCtx -> do
            case a0tye1 of
              A0TyArrow (x11opt, _a0tye11) a0tye12 -> do
                let a0tye12' =
                      case x11opt of
                        Just x11 -> subst0 a0e2 x11 a0tye12
                        Nothing -> a0tye12
                pure (a0tye12', A0App a0e1 (applyCast a0eCastOpt a0e2), retAppCtx)
              _ -> do
                let Expr loc1 _ = e1
                spanInFile1 <- askSpanInFile loc1
                typeError trav $ NotAFunctionTypeForStage0 spanInFile1 a0tye1
          _ ->
            error "bug: App, fun, not a RetCast0"
      LamOpt (x1, tye1) e2 -> do
        case appCtx of
          [] -> do
            a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
            (a0tye2, a0e2, retAppCtx1) <-
              typecheckExpr0 trav (TypeEnv.addVar x1 (TypeEnv.Ass0Entry a0tye1) tyEnv) [] e2
            validateEmptyRetAppContext "stage-1, Lam, non-rec" retAppCtx1
            let ax1 = AssVar x1
            pure (A0TyOptArrow (ax1, a0tye1) a0tye2, A0Lam Nothing (ax1, a0tye1) a0e2, [])
          _ : _ ->
            error "TODO: stage-0, LamOpt, non-empty AppContext"
      AppOptGiven e1 e2 -> do
        (a0tye2, a0e2, retAppCtx2) <- typecheckExpr0 trav tyEnv [] e2
        validateEmptyRetAppContext "stage-0, AppOpt, arg" retAppCtx2
        (a0tye1, a0e1, retAppCtx1) <- typecheckExpr0 trav tyEnv (AppArgOpt0 a0e2 a0tye2 : appCtx) e1
        case retAppCtx1 of
          RetCast0 a0eCastOpt : retAppCtx -> do
            case a0tye1 of
              A0TyOptArrow (x11, _a0tye11) a0tye12 -> do
                pure (subst0 a0e2 x11 a0tye12, A0App a0e1 (applyCast a0eCastOpt a0e2), retAppCtx)
              _ -> do
                let Expr loc1 _ = e1
                spanInFile1 <- askSpanInFile loc1
                typeError trav $ NotAFunctionTypeForStage0 spanInFile1 a0tye1
          _ ->
            error "bug: AppOpt, fun, not a RetCast0"
      AppOptOmitted _e1 -> do
        error "TODO: stage-0, AppOptOmitted"
      LetIn x e1 e2 -> do
        (a0tye1, a0e1, retAppCtx1) <- typecheckExpr0 trav tyEnv [] e1
        case retAppCtx1 of
          [] -> do
            (a0tye2, a0e2, retAppCtx) <-
              typecheckExpr0 trav (TypeEnv.addVar x (TypeEnv.Ass0Entry a0tye1) tyEnv) appCtx e2
            let ax = AssVar x
            if ax `occurs0` a0tye2
              then typeError trav $ VarOccursFreelyInAss0Type spanInFile x a0tye2
              else pure (a0tye2, A0LetIn (ax, a0tye1) a0e1 a0e2, retAppCtx)
          _ : _ ->
            error "bug: LetIn, non-empty RetAppContext"
      IfThenElse e0 e1 e2 -> do
        (a0tye0, a0e0, retAppCtx0) <- typecheckExpr0 trav tyEnv [] e0
        validateEmptyRetAppContext "stage-0, IfThenElse, condition" retAppCtx0
        case a0tye0 of
          A0TyPrim A0TyBool -> do
            (a0tye1, a0e1, retAppCtx1) <- typecheckExpr0 trav tyEnv appCtx e1
            (a0tye2, a0e2, retAppCtx2) <- typecheckExpr0 trav tyEnv appCtx e2
            a0tye <- unifyTypesByConditional trav loc a0e0 a0tye1 a0tye2
            retAppCtx <- unifyRetAppContextsByConditional trav loc a0e0 retAppCtx1 retAppCtx2
            pure (a0tye, A0IfThenElse a0e0 a0e1 a0e2, retAppCtx)
          _ -> do
            let Expr loc0 _ = e0
            spanInFile0 <- askSpanInFile loc0
            typeError trav $ NotABoolTypeForStage0 spanInFile0 a0tye0
      As e1 tye2 -> do
        (a0tye1, a0e1, retAppCtx) <- typecheckExpr0 trav tyEnv appCtx e1
        a0tye2 <- typecheckTypeExpr0 trav tyEnv tye2
        (a0eCastOpt, _) <- makeAssertiveCast trav loc Set.empty a0tye1 a0tye2
        pure (a0tye2, applyCast a0eCastOpt a0e1, retAppCtx)
      Bracket e1 -> do
        (a1tye1, a1e1, retAppCtx) <- typecheckExpr1 trav tyEnv appCtx e1
        pure (A0TyCode a1tye1, A0Bracket a1e1, retAppCtx)
      Escape _ ->
        typeError trav $ CannotUseEscapeAtStage0 spanInFile
  where
    completeInferredOptional triple@(a0tye, a0e, retAppCtx) =
      case retAppCtx of
        RetInferred0 a0eInferred : retAppCtxRest ->
          case a0tye of
            A0TyOptArrow _ a0tye2 ->
              completeInferredOptional (a0tye2, A0App a0e a0eInferred, retAppCtxRest)
            _ ->
              error "bug: completeInferredOptional"
        _ ->
          triple

typecheckExpr1 :: trav -> TypeEnv -> AppContext -> Expr -> M trav (Ass1TypeExpr, Ass1Expr, RetAppContext)
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
              LitVec ns -> do
                let vec = Vector.fromList ns
                pure (A1TyPrim (A1TyVec (A0Literal (ALitInt (Vector.length vec)))), ALitVec vec)
              LitMat nss -> do
                mat <- lift . mapLeft (\e -> (InvalidMatrixLiteral spanInFile e, trav)) $ Matrix.fromRows nss
                pure (A1TyPrim (uncurry A1TyMat (both (A0Literal . ALitInt) (Matrix.size mat))), ALitMat mat)
          pure (a1tye, A1Literal alit, [])
        _ : _ ->
          typeError trav $ CannotApplyLiteral spanInFile
    Var x -> do
      entry <- findVar trav loc x tyEnv
      let ax = AssVar x
      case entry of
        TypeEnv.Ass0Entry _ ->
          typeError trav $ NotAStage1Var spanInFile x
        TypeEnv.Ass1Entry a1tye -> do
          (a1tyeInst, retAppCtx, _) <- instantiateGuidedByAppContext1 trav loc Set.empty appCtx a1tye
          pure (a1tyeInst, A1Var ax, retAppCtx)
    Lam recOpt (x1, tye1) e2 ->
      case appCtx of
        [] ->
          case recOpt of
            Nothing -> do
              a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
              (a1tye2, a1e2, retAppCtx1) <-
                typecheckExpr1 trav (TypeEnv.addVar x1 (TypeEnv.Ass1Entry a1tye1) tyEnv) [] e2
              validateEmptyRetAppContext "stage-1, Lam, non-rec" retAppCtx1
              let ax1 = AssVar x1
              pure (A1TyArrow a1tye1 a1tye2, A1Lam Nothing (ax1, a1tye1) a1e2, [])
            Just (f, tyeRec) -> do
              a1tyeRec <- typecheckTypeExpr1 trav tyEnv tyeRec
              a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
              (a1tye2, a1e2, retAppCtx1) <- do
                let tyEnv' =
                      tyEnv
                        & TypeEnv.addVar x1 (TypeEnv.Ass1Entry a1tye1)
                        & TypeEnv.addVar f (TypeEnv.Ass1Entry a1tyeRec)
                typecheckExpr1 trav tyEnv' [] e2
              validateEmptyRetAppContext "stage-1, Lam, rec" retAppCtx1
              let ax1 = AssVar x1
              let af = AssVar f
              let a1tyeSynth = A1TyArrow a1tye1 a1tye2
              (ty1eqOpt, _) <- makeEquation1 trav loc Set.empty a1tyeSynth a1tyeRec
              pure (a1tyeRec, applyEquationCast loc ty1eqOpt (A1Lam (Just (af, a1tyeRec)) (ax1, a1tye1) a1e2), [])
        _ : _ ->
          error "TODO: stage-1, Lam, non-empty AppContext"
    App e1 e2 -> do
      (a1tye2, a1e2, retAppCtx2) <- typecheckExpr1 trav tyEnv [] e2
      validateEmptyRetAppContext "stage-1, App, arg" retAppCtx2
      (a1tye1, a1e1, retAppCtx1) <- typecheckExpr1 trav tyEnv (AppArg1 a1tye2 : appCtx) e1
      case retAppCtx1 of
        RetCast1 ty1eq : retAppCtx ->
          case a1tye1 of
            A1TyArrow _a1tye11 a1tye12 -> do
              -- Embeds type equality assertion at stage 0 here!
              -- ty1eq <- makeEquation1 trav loc a1tye2 a1tye11
              pure (a1tye12, A1App a1e1 (applyEquationCast loc ty1eq a1e2), retAppCtx)
            _ -> do
              let Expr loc1 _ = e1
              spanInFile1 <- askSpanInFile loc1
              typeError trav $ NotAFunctionTypeForStage1 spanInFile1 a1tye1
        _ ->
          error "bug: stage-1, App, fun, not a RetCast1"
    LamOpt _ _ ->
      error "TODO: stage-1, LamOpt, error"
    AppOptGiven _ _ ->
      error "TODO: stage-1, AppOpt, error"
    AppOptOmitted _ ->
      error "TODO: stage-1, AppOptOmitted, error"
    LetIn x e1 e2 -> do
      (a1tye1, a1e1, retAppCtx1) <- typecheckExpr1 trav tyEnv [] e1
      validateEmptyRetAppContext "stage-1, LetIn" retAppCtx1
      (a1tye2, a1e2, retAppCtx2) <-
        typecheckExpr1 trav (TypeEnv.addVar x (TypeEnv.Ass1Entry a1tye1) tyEnv) appCtx e2
      let ax = AssVar x
      if ax `occurs0` a1tye2
        then typeError trav $ VarOccursFreelyInAss1Type spanInFile x a1tye2
        else pure (a1tye2, A1App (A1Lam Nothing (ax, a1tye1) a1e2) a1e1, retAppCtx2)
    IfThenElse e0 e1 e2 -> do
      (a1tye0, a1e0, retAppCtx0) <- typecheckExpr1 trav tyEnv [] e0
      validateEmptyRetAppContext "stage-1, IfThenElse" retAppCtx0
      case a1tye0 of
        A1TyPrim A1TyBool -> do
          (a1tye1, a1e1, _retAppCtx1) <- typecheckExpr1 trav tyEnv appCtx e1
          (a1tye2, a1e2, _retAppCtx2) <- typecheckExpr1 trav tyEnv appCtx e2
          (ty1eqOpt, _) <- makeEquation1 trav loc Set.empty a1tye2 a1tye1
          let retAppCtx = error "TODO: stage-1, IfThenElse, retAppCtx"
          pure (a1tye1, A1IfThenElse a1e0 a1e1 (applyEquationCast loc ty1eqOpt a1e2), retAppCtx)
        _ -> do
          let Expr loc0 _ = e0
          spanInFile0 <- askSpanInFile loc0
          typeError trav $ NotABoolTypeForStage1 spanInFile0 a1tye0
    As e1 tye2 -> do
      (a1tye1, a1e1, retAppCtx) <- typecheckExpr1 trav tyEnv appCtx e1
      a1tye2 <- typecheckTypeExpr1 trav tyEnv tye2
      (ty1eqOpt, _) <- makeEquation1 trav loc Set.empty a1tye1 a1tye2
      pure (a1tye2, applyEquationCast loc ty1eqOpt a1e1, retAppCtx)
    Bracket _ ->
      typeError trav $ CannotUseBracketAtStage1 spanInFile
    Escape e1 -> do
      (a0tye1, a0e1, retAppCtx) <- typecheckExpr0 trav tyEnv appCtx e1
      case a0tye1 of
        A0TyCode a1tye ->
          pure (a1tye, A1Escape a0e1, retAppCtx)
        _ -> do
          let Expr loc1 _ = e1
          spanInFile1 <- askSpanInFile loc1
          typeError trav $ NotACodeType spanInFile1 a0tye1

insertCastForNatArg :: trav -> (Ass0TypeExpr, Ass0Expr, Span) -> M trav Ass0Expr
insertCastForNatArg trav (a0tye, a0e, loc) = do
  (a0eCastOpt, _) <- makeAssertiveCast trav loc Set.empty a0tye (A0TyPrim A0TyNat)
  pure $ applyCast a0eCastOpt a0e

validateIntLiteral :: trav -> (Ass0Expr, Span) -> M trav Int
validateIntLiteral trav = \case
  (A0Literal (ALitInt n), _) ->
    pure n
  (a0e, loc) -> do
    spanInFile <- askSpanInFile loc
    typeError trav $ NotAnIntLitArgAtStage0 spanInFile a0e

typecheckTypeExpr0 :: trav -> TypeEnv -> TypeExpr -> M trav Ass0TypeExpr
typecheckTypeExpr0 trav tyEnv (TypeExpr loc tyeMain) = do
  spanInFile <- askSpanInFile loc
  case tyeMain of
    TyName tyName args -> do
      results <-
        mapM
          ( \case
              PersistentArg e -> do
                let Expr loc' _ = e
                spanInFile' <- askSpanInFile loc'
                typeError trav $ CannotUsePersistentArgAtStage0 spanInFile'
              NormalArg e -> do
                let Expr loc' _ = e
                (_a0tye, a0e, retAppCtx) <- typecheckExpr0 trav tyEnv [] e
                validateEmptyRetAppContext "NormalArg" retAppCtx
                pure (a0e, loc')
          )
          args
      tyPrim <-
        case (tyName, results) of
          ("Int", []) -> pure A0TyInt
          ("Nat", []) -> pure A0TyNat
          ("Bool", []) -> pure A0TyBool
          ("Vec", [arg]) -> do
            n <- validateIntLiteral trav arg
            pure $ A0TyVec n
          ("Mat", [arg1, arg2]) -> do
            m <- validateIntLiteral trav arg1
            n <- validateIntLiteral trav arg2
            pure $ A0TyMat m n
          _ -> typeError trav $ UnknownTypeOrInvalidArityAtStage0 spanInFile tyName (List.length results)
      pure $ A0TyPrim tyPrim
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

typecheckTypeExpr1 :: trav -> TypeEnv -> TypeExpr -> M trav Ass1TypeExpr
typecheckTypeExpr1 trav tyEnv (TypeExpr loc tyeMain) = do
  spanInFile <- askSpanInFile loc
  case tyeMain of
    TyName tyName args -> do
      results <-
        mapM
          ( \case
              PersistentArg e -> do
                let Expr loc' _ = e
                (a0tye, a0e, retAppCtx) <- typecheckExpr0 trav tyEnv [] e
                validateEmptyRetAppContext "PersistentArg" retAppCtx
                pure (a0tye, a0e, loc')
              NormalArg e -> do
                let Expr loc' _ = e
                spanInFile' <- askSpanInFile loc'
                typeError trav $ CannotUseNormalArgAtStage1 spanInFile'
          )
          args
      a1tyPrim <-
        case (tyName, results) of
          ("Int", []) -> pure A1TyInt
          ("Bool", []) -> pure A1TyBool
          ("Vec", [arg]) -> do
            a0e <- insertCastForNatArg trav arg
            pure $ A1TyVec a0e
          ("Mat", [arg1, arg2]) -> do
            a0e1 <- insertCastForNatArg trav arg1
            a0e2 <- insertCastForNatArg trav arg2
            pure $ A1TyMat a0e1 a0e2
          _ -> typeError trav $ UnknownTypeOrInvalidArityAtStage1 spanInFile tyName (List.length results)
      pure $ A1TyPrim a1tyPrim
    TyArrow (xOpt, tye1) tye2 -> do
      a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
      () <-
        case xOpt of
          Just x -> typeError trav $ FunctionTypeCannotBeDependentAtStage1 spanInFile x
          Nothing -> pure ()
      a1tye2 <- typecheckTypeExpr1 trav tyEnv tye2
      pure $ A1TyArrow a1tye1 a1tye2
    TyOptArrow _ _ ->
      error "TODO: stage-1, TyOptArrow, error"
    TyCode _ -> do
      typeError trav $ CannotUseCodeTypeAtStage1 spanInFile
