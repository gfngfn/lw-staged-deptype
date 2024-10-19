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
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Tuple.Extra
import Lwsd.BuiltIn (ass0exprAssertNat)
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

generateFreshVar :: M trav Var
generateFreshVar = do
  currentState@TypecheckState {nextVarIndex} <- get
  put $ currentState {nextVarIndex = nextVarIndex + 1}
  pure $ Text.pack ("#X" ++ show nextVarIndex)

makeIdentityLam :: Ass0TypeExpr -> M trav Ass0Expr
makeIdentityLam a0tye = do
  x <- generateFreshVar
  pure $ A0Lam Nothing (x, a0tye) (A0Var x)

applyCast :: Maybe Ass0Expr -> Ass0Expr -> Ass0Expr
applyCast = maybe id A0App

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
          ty1eq <- makeEquation1 trav loc a1tye1 a1tye2
          pure $ Just $ A0TyEqAssert loc ty1eq -- TODO: optimize this
        _ ->
          typeError trav $ TypeContradictionAtStage0 spanInFile a0tye1 a0tye2

    nothingOrIdentityLam :: Ass0TypeExpr -> M trav (Maybe Ass0Expr)
    nothingOrIdentityLam a0tye = do
      TypecheckState {optimizeTrivialAssertion} <- get
      if optimizeTrivialAssertion
        then pure Nothing
        else Just <$> makeIdentityLam a0tye

makeEquation1 :: trav -> Span -> Ass1TypeExpr -> Ass1TypeExpr -> M trav Type1Equation
makeEquation1 trav loc a1tye1' a1tye2' = do
  spanInFile <- askSpanInFile loc
  case go a1tye1' a1tye2' of
    Nothing -> typeError trav $ TypeContradictionAtStage1 spanInFile a1tye1' a1tye2'
    Just ty1eq -> pure ty1eq
  where
    go :: Ass1TypeExpr -> Ass1TypeExpr -> Maybe Type1Equation
    go a1tye1 a1tye2 =
      case (a1tye1, a1tye2) of
        (A1TyPrim a1tyPrim1, A1TyPrim a1tyPrim2) ->
          TyEq1Prim
            <$> case (a1tyPrim1, a1tyPrim2) of
              (A1TyInt, A1TyInt) -> pure TyEq1Int
              (A1TyBool, A1TyBool) -> pure TyEq1Bool
              (A1TyVec a0e1, A1TyVec a0e2) -> pure $ TyEq1Vec a0e1 a0e2
              (A1TyMat a0e11 a0e12, A1TyMat a0e21 a0e22) -> pure $ TyEq1Mat a0e11 a0e12 a0e21 a0e22
              (_, _) -> Nothing
        (A1TyArrow a1tye11 a1tye12, A1TyArrow a1tye21 a1tye22) -> do
          ty1eqDom <- go a1tye11 a1tye21
          ty1eqCod <- go a1tye12 a1tye22
          pure $ TyEq1Arrow ty1eqDom ty1eqCod
        (_, _) ->
          Nothing

typecheckExpr0 :: trav -> TypeEnv -> Expr -> M trav (Ass0TypeExpr, Ass0Expr)
typecheckExpr0 trav tyEnv (Expr loc eMain) = do
  spanInFile <- askSpanInFile loc
  case eMain of
    Literal lit -> do
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
      pure (a0tye, A0Literal alit)
    Var x -> do
      entry <- findVar trav loc x tyEnv
      case entry of
        TypeEnv.Ass0Entry a0tye -> pure (a0tye, A0Var x)
        TypeEnv.Ass1Entry _ -> typeError trav $ NotAStage0Var spanInFile x
    Lam Nothing (x1, tye1) e2 -> do
      a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
      (a0tye2, a0e2) <- typecheckExpr0 trav (TypeEnv.addVar x1 (TypeEnv.Ass0Entry a0tye1) tyEnv) e2
      pure (A0TyArrow (Just x1, a0tye1) a0tye2, A0Lam Nothing (x1, a0tye1) a0e2)
    Lam (Just (f, tyeRec)) (x1, tye1) e2 -> do
      a0tyeRec <- typecheckTypeExpr0 trav tyEnv tyeRec
      a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
      (a0tye2, a0e2) <-
        typecheckExpr0
          trav
          (TypeEnv.addVar f (TypeEnv.Ass0Entry a0tyeRec) (TypeEnv.addVar x1 (TypeEnv.Ass0Entry a0tye1) tyEnv))
          e2
      let a0tyeSynth = A0TyArrow (Just x1, a0tye1) a0tye2
      a0eCastOpt <- makeAssertiveCast trav loc a0tyeSynth a0tyeRec
      pure (a0tyeRec, applyCast a0eCastOpt (A0Lam (Just (f, a0tyeRec)) (x1, a0tye1) a0e2))
    App e1 e2 -> do
      (a0tye1, a0e1) <- typecheckExpr0 trav tyEnv e1
      (a0tye2, a0e2) <- typecheckExpr0 trav tyEnv e2
      case a0tye1 of
        A0TyArrow (x11opt, a0tye11) a0tye12 -> do
          a0eCastOpt <- makeAssertiveCast trav loc a0tye2 a0tye11
          let a0tye12' =
                case x11opt of
                  Just x11 -> subst0 a0e2 x11 a0tye12
                  Nothing -> a0tye12
          pure (a0tye12', A0App a0e1 (applyCast a0eCastOpt a0e2))
        _ -> do
          let Expr loc1 _ = e1
          spanInFile1 <- askSpanInFile loc1
          typeError trav $ NotAFunctionTypeForStage0 spanInFile1 a0tye1
    LetIn x e1 e2 -> do
      (a0tye1, a0e1) <- typecheckExpr0 trav tyEnv e1
      (a0tye2, a0e2) <- typecheckExpr0 trav (TypeEnv.addVar x (TypeEnv.Ass0Entry a0tye1) tyEnv) e2
      if x `occurs0` a0tye2
        then typeError trav $ VarOccursFreelyInAss0Type spanInFile x a0tye2
        else pure (a0tye2, A0App (A0Lam Nothing (x, a0tye1) a0e2) a0e1)
    IfThenElse e0 e1 e2 -> do
      (a0tye0, a0e0) <- typecheckExpr0 trav tyEnv e0
      case a0tye0 of
        A0TyPrim A0TyBool -> do
          (a0tye1, a0e1) <- typecheckExpr0 trav tyEnv e1
          (a0tye2, a0e2) <- typecheckExpr0 trav tyEnv e2
          a0eCastOpt <- makeAssertiveCast trav loc a0tye2 a0tye1
          pure (a0tye1, A0IfThenElse a0e0 a0e1 (applyCast a0eCastOpt a0e2))
        _ -> do
          let Expr loc0 _ = e0
          spanInFile0 <- askSpanInFile loc0
          typeError trav $ NotABoolTypeForStage0 spanInFile0 a0tye0
    As e1 tye2 -> do
      (a0tye1, a0e1) <- typecheckExpr0 trav tyEnv e1
      a0tye2 <- typecheckTypeExpr0 trav tyEnv tye2
      a0eCastOpt <- makeAssertiveCast trav loc a0tye1 a0tye2
      pure (a0tye2, applyCast a0eCastOpt a0e1)
    Bracket e1 -> do
      (a1tye1, a1e1) <- typecheckExpr1 trav tyEnv e1
      pure (A0TyCode a1tye1, A0Bracket a1e1)
    Escape _ ->
      typeError trav $ CannotUseEscapeAtStage0 spanInFile

typecheckExpr1 :: trav -> TypeEnv -> Expr -> M trav (Ass1TypeExpr, Ass1Expr)
typecheckExpr1 trav tyEnv (Expr loc eMain) = do
  TypecheckState {optimizeTrivialAssertion} <- get
  spanInFile <- askSpanInFile loc
  case eMain of
    Literal lit -> do
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
      pure (a1tye, A1Literal alit)
    Var x -> do
      entry <- findVar trav loc x tyEnv
      case entry of
        TypeEnv.Ass0Entry _ -> typeError trav $ NotAStage1Var spanInFile x
        TypeEnv.Ass1Entry a1tye -> pure (a1tye, A1Var x)
    Lam Nothing (x1, tye1) e2 -> do
      a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
      (a1tye2, a1e2) <- typecheckExpr1 trav (TypeEnv.addVar x1 (TypeEnv.Ass1Entry a1tye1) tyEnv) e2
      pure (A1TyArrow a1tye1 a1tye2, A1Lam Nothing (x1, a1tye1) a1e2)
    Lam (Just (f, tyeRec)) (x1, tye1) e2 -> do
      a1tyeRec <- typecheckTypeExpr1 trav tyEnv tyeRec
      a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
      (a1tye2, a1e2) <-
        typecheckExpr1
          trav
          (TypeEnv.addVar f (TypeEnv.Ass1Entry a1tyeRec) (TypeEnv.addVar x1 (TypeEnv.Ass1Entry a1tye1) tyEnv))
          e2
      let a1tyeSynth = A1TyArrow a1tye1 a1tye2
      ty1eq <- makeEquation1 trav loc a1tyeSynth a1tyeRec
      let a1eLam = A1Lam (Just (f, a1tyeRec)) (x1, a1tye1) a1e2
          a1e =
            if optimizeTrivialAssertion && alphaEquivalent a1tyeSynth a1tyeRec
              then a1eLam -- Do slight shortcuts
              else A1Escape (A0App (A0TyEqAssert loc ty1eq) (A0Bracket a1eLam))
      pure (a1tyeRec, a1e)
    App e1 e2 -> do
      (a1tye1, a1e1) <- typecheckExpr1 trav tyEnv e1
      (a1tye2, a1e2) <- typecheckExpr1 trav tyEnv e2
      case a1tye1 of
        A1TyArrow a1tye11 a1tye12 -> do
          -- Embeds type equality assertion at stage 0 here!
          ty1eq <- makeEquation1 trav loc a1tye2 a1tye11
          let a1e2' =
                if optimizeTrivialAssertion && alphaEquivalent a1tye2 a1tye11
                  then a1e2 -- Do slight shortcuts
                  else A1Escape (A0App (A0TyEqAssert loc ty1eq) (A0Bracket a1e2))
          pure (a1tye12, A1App a1e1 a1e2')
        _ -> do
          let Expr loc1 _ = e1
          spanInFile1 <- askSpanInFile loc1
          typeError trav $ NotAFunctionTypeForStage1 spanInFile1 a1tye1
    LetIn x e1 e2 -> do
      (a1tye1, a1e1) <- typecheckExpr1 trav tyEnv e1
      (a1tye2, a1e2) <- typecheckExpr1 trav (TypeEnv.addVar x (TypeEnv.Ass1Entry a1tye1) tyEnv) e2
      if x `occurs0` a1tye2
        then typeError trav $ VarOccursFreelyInAss1Type spanInFile x a1tye2
        else pure (a1tye2, A1App (A1Lam Nothing (x, a1tye1) a1e2) a1e1)
    IfThenElse e0 e1 e2 -> do
      (a1tye0, a1e0) <- typecheckExpr1 trav tyEnv e0
      case a1tye0 of
        A1TyPrim A1TyBool -> do
          (a1tye1, a1e1) <- typecheckExpr1 trav tyEnv e1
          (a1tye2, a1e2) <- typecheckExpr1 trav tyEnv e2
          ty1eq <- makeEquation1 trav loc a1tye2 a1tye1
          let a1e2' =
                if optimizeTrivialAssertion && alphaEquivalent a1tye2 a1tye1
                  then a1e2
                  else A1Escape (A0App (A0TyEqAssert loc ty1eq) (A0Bracket a1e2))
          pure (a1tye1, A1IfThenElse a1e0 a1e1 a1e2')
        _ -> do
          let Expr loc0 _ = e0
          spanInFile0 <- askSpanInFile loc0
          typeError trav $ NotABoolTypeForStage1 spanInFile0 a1tye0
    As e1 tye2 -> do
      (a1tye1, a1e1) <- typecheckExpr1 trav tyEnv e1
      a1tye2 <- typecheckTypeExpr1 trav tyEnv tye2
      ty1eq <- makeEquation1 trav loc a1tye1 a1tye2
      let a1e =
            if optimizeTrivialAssertion && alphaEquivalent a1tye1 a1tye2
              then a1e1
              else A1Escape (A0App (A0TyEqAssert loc ty1eq) (A0Bracket a1e1))
      pure (a1tye2, a1e)
    Bracket _ ->
      typeError trav $ CannotUseBracketAtStage1 spanInFile
    Escape e1 -> do
      (a0tye1, a0e1) <- typecheckExpr0 trav tyEnv e1
      case a0tye1 of
        A0TyCode a1tye ->
          pure (a1tye, A1Escape a0e1)
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
                (_a0tye, a0e) <- typecheckExpr0 trav tyEnv e
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
      pure $ A0TyArrow (xOpt, a0tye1) a0tye2
    TyCode tye1 -> do
      a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
      pure $ A0TyCode a1tye1

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
                (a0tye, a0e) <- typecheckExpr0 trav tyEnv e
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
    TyCode _ -> do
      typeError trav $ CannotUseCodeTypeAtStage1 spanInFile
