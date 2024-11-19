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
makeAssertiveCast :: forall trav. trav -> Span -> Ass0TypeExpr -> Ass0TypeExpr -> M trav (Maybe Ass0Expr)
makeAssertiveCast trav loc =
  go
  where
    go :: Ass0TypeExpr -> Ass0TypeExpr -> M trav (Maybe Ass0Expr)
    go a0tye1 a0tye2
      | alphaEquivalent a0tye1 a0tye2 =
          pure Nothing
    go a0tye1 a0tye2 = do
      spanInFile <- askSpanInFile loc
      case (a0tye1, a0tye2) of
        (A0TyPrim a0tyPrim1, A0TyPrim a0tyPrim2) -> do
          case (a0tyPrim1, a0tyPrim2) of
            (A0TyInt, A0TyInt) -> nothingOrIdentityLam (A0TyPrim A0TyInt)
            (A0TyNat, A0TyNat) -> nothingOrIdentityLam (A0TyPrim A0TyNat)
            (A0TyNat, A0TyInt) -> nothingOrIdentityLam (A0TyPrim A0TyInt) -- Implicit upcast
            (A0TyInt, A0TyNat) -> pure $ Just $ ass0exprAssertNat loc -- Assertive downcast
            (A0TyBool, A0TyBool) -> nothingOrIdentityLam (A0TyPrim A0TyBool)
            (A0TyVec n1, A0TyVec n2) | n1 == n2 -> nothingOrIdentityLam (A0TyPrim (A0TyVec n1))
            (A0TyMat m1 n1, A0TyMat m2 n2) | m1 == m2 && n1 == n2 -> nothingOrIdentityLam (A0TyPrim (A0TyMat m1 n1))
            _ -> typeError trav $ TypeContradictionAtStage0 spanInFile a0tye1 a0tye2
        (A0TyArrow (x1opt, a0tye11) a0tye12, A0TyArrow (x2opt, a0tye21) a0tye22) -> do
          a0eDomCastOpt <- go a0tye11 a0tye21
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
          a0eCodCastOpt <- makeAssertiveCast trav loc a0tye12 a0tye22'
          f <- generateFreshVar
          x' <- generateFreshVar
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
        (A0TyCode a1tye1, A0TyCode a1tye2) -> do
          ty1eqOpt <- makeEquation1 trav loc a1tye1 a1tye2
          pure $ fmap (A0TyEqAssert loc) ty1eqOpt
        _ ->
          typeError trav $ TypeContradictionAtStage0 spanInFile a0tye1 a0tye2

    nothingOrIdentityLam :: Ass0TypeExpr -> M trav (Maybe Ass0Expr)
    nothingOrIdentityLam a0tye = do
      TypecheckState {optimizeTrivialAssertion} <- get
      if optimizeTrivialAssertion
        then pure Nothing
        else Just <$> makeIdentityLam a0tye

makeEquation1 :: forall trav. trav -> Span -> Ass1TypeExpr -> Ass1TypeExpr -> M trav (Maybe Type1Equation)
makeEquation1 trav loc a1tye1' a1tye2' = do
  TypecheckState {optimizeTrivialAssertion} <- get
  spanInFile <- askSpanInFile loc
  case go a1tye1' a1tye2' of
    Right (trivial, ty1eq) ->
      if trivial && optimizeTrivialAssertion
        then pure Nothing
        else pure $ Just ty1eq
    Left () ->
      typeError trav $ TypeContradictionAtStage1 spanInFile a1tye1' a1tye2'
  where
    go :: Ass1TypeExpr -> Ass1TypeExpr -> Either () (Bool, Type1Equation)
    go a1tye1 a1tye2 =
      case (a1tye1, a1tye2) of
        (A1TyPrim a1tyPrim1, A1TyPrim a1tyPrim2) ->
          case (a1tyPrim1, a1tyPrim2) of
            (A1TyInt, A1TyInt) ->
              pure (True, TyEq1Prim TyEq1Int)
            (A1TyBool, A1TyBool) ->
              pure (True, TyEq1Prim TyEq1Bool)
            (A1TyVec a0e1, A1TyVec a0e2) -> do
              let trivial = alphaEquivalent a0e1 a0e2
              pure (trivial, TyEq1Prim (TyEq1Vec a0e1 a0e2))
            (A1TyMat a0e11 a0e12, A1TyMat a0e21 a0e22) -> do
              let trivial1 = alphaEquivalent a0e11 a0e21
              let trivial2 = alphaEquivalent a0e11 a0e21
              pure (trivial1 && trivial2, TyEq1Prim (TyEq1Mat a0e11 a0e12 a0e21 a0e22))
            (_, _) -> Left ()
        (A1TyArrow a1tye11 a1tye12, A1TyArrow a1tye21 a1tye22) -> do
          (trivial1, ty1eqDom) <- go a1tye11 a1tye21
          (trivial2, ty1eqCod) <- go a1tye12 a1tye22
          pure (trivial1 && trivial2, TyEq1Arrow ty1eqDom ty1eqCod)
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

instantiateGuidedByAppContext0 :: forall trav. trav -> Span -> AppContext -> Ass0TypeExpr -> M trav (Ass0TypeExpr, RetAppContext)
instantiateGuidedByAppContext0 trav loc = go
  where
    go :: AppContext -> Ass0TypeExpr -> M trav (Ass0TypeExpr, RetAppContext)
    go appCtx a0tye =
      case (appCtx, a0tye) of
        ([], _) ->
          pure (a0tye, [])
        (AppArg0 a0e1' a0tye1' : appCtx', A0TyArrow (xOpt, a0tye1) a0tye2) -> do
          a0eCastOpt <- makeAssertiveCast trav loc a0tye1' a0tye1
          (a0tye2', retAppCtx') <-
            case xOpt of
              Nothing -> go appCtx' a0tye2
              Just x -> go appCtx' (subst0 a0e1' x a0tye2)
          pure (A0TyArrow (xOpt, a0tye1) a0tye2', RetCast0 a0eCastOpt : retAppCtx')
        (AppArg0 _a0e1' _a0tye1' : _appCtx', A0TyOptArrow (x, a0tye1) a0tye2) -> do
          (a0tye2', retAppCtx') <- go appCtx a0tye2
          let a0eInferred = error "TODO: find a term that can substitute `x` through the traversal of `a0tye2`"
          pure (A0TyOptArrow (x, a0tye1) a0tye2', RetInferred0 a0eInferred : retAppCtx')
        (_, A0TyCode a1tye) -> do
          (a1tye', retAppCtx) <- instantiateGuidedByAppContext1 trav loc appCtx a1tye
          pure (A0TyCode a1tye', retAppCtx)
        _ -> do
          spanInFile <- askSpanInFile loc
          typeError trav $ CannotInstantiateGuidedByAppContext0 spanInFile appCtx a0tye

instantiateGuidedByAppContext1 :: forall trav. trav -> Span -> AppContext -> Ass1TypeExpr -> M trav (Ass1TypeExpr, RetAppContext)
instantiateGuidedByAppContext1 trav loc = go
  where
    go :: AppContext -> Ass1TypeExpr -> M trav (Ass1TypeExpr, RetAppContext)
    go appCtx a1tye =
      case (appCtx, a1tye) of
        ([], _) ->
          pure (a1tye, [])
        (AppArg1 a1tye1' : appCtx', A1TyArrow a1tye1 a1tye2) -> do
          ty1eq <- makeEquation1 trav loc a1tye1' a1tye1
          (a1tye2', retAppCtx') <- go appCtx' a1tye2
          pure (A1TyArrow a1tye1 a1tye2', RetCast1 ty1eq : retAppCtx')
        _ ->
          error "TODO: instantiateGuidedByAppContext1, error"

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
                a0eCastOpt <- makeAssertiveCast trav loc a0tyeSynth a0tyeRec
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
                -- a0eCastOpt <- makeAssertiveCast trav loc a0tye2 a0tye11
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
            error "TODO: stage-1, LamOpt, non-empty AppContext"
      LetIn x e1 e2 -> do
        (a0tye1, a0e1, retAppCtx1) <- typecheckExpr0 trav tyEnv [] e1
        case retAppCtx1 of
          [] -> do
            (a0tye2, a0e2, retAppCtx) <-
              typecheckExpr0 trav (TypeEnv.addVar x (TypeEnv.Ass0Entry a0tye1) tyEnv) appCtx e2
            let ax = AssVar x
            if ax `occurs0` a0tye2
              then typeError trav $ VarOccursFreelyInAss0Type spanInFile x a0tye2
              else pure (a0tye2, A0App (A0Lam Nothing (ax, a0tye1) a0e2) a0e1, retAppCtx)
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
        a0eCastOpt <- makeAssertiveCast trav loc a0tye1 a0tye2
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
          (a1tyeInst, retAppCtx) <- instantiateGuidedByAppContext1 trav loc appCtx a1tye
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
              ty1eqOpt <- makeEquation1 trav loc a1tyeSynth a1tyeRec
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
          ty1eqOpt <- makeEquation1 trav loc a1tye2 a1tye1
          let retAppCtx = error "TODO: stage-1, IfThenElse, retAppCtx"
          pure (a1tye1, A1IfThenElse a1e0 a1e1 (applyEquationCast loc ty1eqOpt a1e2), retAppCtx)
        _ -> do
          let Expr loc0 _ = e0
          spanInFile0 <- askSpanInFile loc0
          typeError trav $ NotABoolTypeForStage1 spanInFile0 a1tye0
    As e1 tye2 -> do
      (a1tye1, a1e1, retAppCtx) <- typecheckExpr1 trav tyEnv appCtx e1
      a1tye2 <- typecheckTypeExpr1 trav tyEnv tye2
      ty1eqOpt <- makeEquation1 trav loc a1tye1 a1tye2
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
  a0eCastOpt <- makeAssertiveCast trav loc a0tye (A0TyPrim A0TyNat)
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
