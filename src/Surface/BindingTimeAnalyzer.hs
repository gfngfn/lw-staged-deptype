module Surface.BindingTimeAnalyzer
  ( AnalysisError (..),
    run,
  )
where

import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Generics.Labels ()
import Data.Map qualified as Map
import Lwsd.Syntax qualified as Lwsd
import Surface.BindingTime.Constraint
import Surface.BindingTime.Core
import Surface.BindingTime.Stager
import Surface.Syntax
import Util.LocationInFile (SourceSpec, SpanInFile, getSpanInFile)
import Util.TokenUtil (Span)
import Prelude hiding (succ)

initialState :: BindingTimeVar
initialState = BindingTimeVar 0

succ :: BindingTimeVar -> BindingTimeVar
succ (BindingTimeVar n) = BindingTimeVar (n + 1)

type Assigner a = State BindingTimeVar a

fresh :: Assigner BindingTimeVar
fresh = do
  btv <- get
  put (succ btv)
  pure btv

assignBindingTimeVarToExpr :: Expr -> Assigner BExpr
assignBindingTimeVarToExpr (Expr ann exprMain) = do
  btv <- fresh
  Expr (BTVar btv, ann)
    <$> case exprMain of
      Literal lit ->
        pure $ Literal lit
      Var x ->
        pure $ Var x
      Lam (x, ty) e -> do
        bty <- assignBindingTimeVarToTypeExpr ty
        be <- assignBindingTimeVarToExpr e
        pure $ Lam (x, bty) be
      App e1 e2 -> do
        be1 <- assignBindingTimeVarToExpr e1
        be2 <- assignBindingTimeVarToExpr e2
        pure $ App be1 be2
      LetIn x e1 e2 -> do
        be1 <- assignBindingTimeVarToExpr e1
        be2 <- assignBindingTimeVarToExpr e2
        pure $ LetIn x be1 be2

assignBindingTimeVarToTypeExpr :: TypeExpr -> Assigner BTypeExpr
assignBindingTimeVarToTypeExpr (TypeExpr ann typeExprMain) = do
  btv <- fresh
  TypeExpr (BTVar btv, ann)
    <$> case typeExprMain of
      TyName tyName es -> do
        bes <- mapM assignBindingTimeVarToExpr es
        pure $ TyName tyName bes
      TyArrow (xOpt, ty1) ty2 -> do
        bty1 <- assignBindingTimeVarToTypeExpr ty1
        bty2 <- assignBindingTimeVarToTypeExpr ty2
        pure $ TyArrow (xOpt, bty1) bty2

data AnalysisError
  = UnboundVar SpanInFile Var
  | NotAFunction SpanInFile BIType
  | BindingTimeContradiction SpanInFile
  deriving stock (Show)

data AnalysisConfig = AnalysisConfig
  { sourceSpec :: SourceSpec
  }

type M a = ReaderT AnalysisConfig (Either AnalysisError) a

analysisError :: AnalysisError -> M a
analysisError = lift . Left

askSpanInFile :: Span -> M SpanInFile
askSpanInFile loc = do
  AnalysisConfig {sourceSpec} <- ask
  pure $ getSpanInFile sourceSpec loc

enhanceBIType :: (a -> (BindingTime, Span)) -> BITypeF a -> BIType
enhanceBIType enh (BIType meta bityMain) =
  BIType (enh meta) $
    case bityMain of
      BITyBase es -> BITyBase (map fExpr es)
      BITyArrow (xOpt, bity1) bity2 -> BITyArrow (xOpt, fBIType bity1) (fBIType bity2)
  where
    fBIType = enhanceBIType enh
    fExpr = enhanceExpr enh

enhanceExpr :: (a -> (BindingTime, Span)) -> ExprF a -> BExpr
enhanceExpr enh (Expr meta exprMain) =
  Expr (enh meta) $
    case exprMain of
      Literal lit -> Literal lit
      Var x -> Var x
      Lam (x, tye1) e2 -> Lam (x, fTypeExpr tye1) (fExpr e2)
      App e1 e2 -> App (fExpr e1) (fExpr e2)
      LetIn x e1 e2 -> LetIn x (fExpr e1) (fExpr e2)
  where
    fExpr = enhanceExpr enh
    fTypeExpr = enhanceTypeExpr enh

enhanceTypeExpr :: (a -> (BindingTime, Span)) -> TypeExprF a -> BTypeExpr
enhanceTypeExpr enh (TypeExpr meta typeExprMain) =
  TypeExpr (enh meta) $
    case typeExprMain of
      TyName tyName args -> TyName tyName (map fExpr args)
      TyArrow (xOpt, tye1) tye2 -> TyArrow (xOpt, fTypeExpr tye1) (fTypeExpr tye2)
  where
    fExpr = enhanceExpr enh
    fTypeExpr = enhanceTypeExpr enh

extractConstraintsFromExpr :: BindingTimeEnv -> BExpr -> M (BExpr, BIType, [Constraint Span])
extractConstraintsFromExpr btenv (Expr (bt, ann) exprMain) = do
  spanInFile <- askSpanInFile ann
  case exprMain of
    Literal lit -> do
      pure (Expr (bt, ann) (Literal lit), BIType (bt, ann) (BITyBase []), [])
    Var x -> do
      (x', bity, constraints) <-
        case Map.lookup x btenv of
          Nothing ->
            analysisError $ UnboundVar spanInFile x
          Just (EntryBuiltInPersistent bityVoid) ->
            pure (x, enhanceBIType (\() -> (bt, ann)) bityVoid, []) -- TODO: refine `ann`
          Just (EntryBuiltInFixed x' btc' bityConst) ->
            pure (x', enhanceBIType (\btc -> (BTConst btc, ann)) bityConst, [CEqual ann bt (BTConst btc')])
          Just (EntryLocallyBound bt' bity) ->
            pure (x, bity, [CEqual ann bt bt'])
      pure (Expr (bt, ann) (Var x'), bity, constraints)
    Lam (x1, btye1) e2 -> do
      (btye1', bity1@(BIType (bt1, _) _), constraints1) <- extractConstraintsFromTypeExpr btenv btye1
      (e2', bity2@(BIType (btv2, _) _), constraints2) <-
        extractConstraintsFromExpr (Map.insert x1 (EntryLocallyBound bt bity1) btenv) e2
      let constraints =
            if occurs x1 bity2
              then [CEqual ann bt (BTConst BT0)]
              else [CLeq ann bt bt1, CLeq ann bt btv2]
      let e' = Expr (bt, ann) (Lam (x1, btye1') e2')
      pure (e', BIType (bt, ann) (BITyArrow (Just x1, bity1) bity2), constraints1 ++ constraints2 ++ constraints)
    App e1 e2 -> do
      (e1', bity1@(BIType (bt1, _) bityMain1), constraints1) <- extractConstraintsFromExpr btenv e1
      (e2', _bity2@(BIType (bt2, _) _bityMain2), constraints2) <- extractConstraintsFromExpr btenv e2
      (bity, constraints) <-
        case bityMain1 of
          BITyArrow (x11opt, _bity11@(BIType (bt11, _) _bityMain11)) bity12 -> do
            let constraints0 = [CEqual ann bt bt1, CEqual ann bt2 bt11]
            let constraints = constraints1 ++ constraints2 ++ constraints0
            case x11opt of
              Just x11 ->
                if occurs x11 bity12
                  then pure (subst e2 x11 bity12, constraints ++ [CEqual ann bt (BTConst BT0)])
                  else pure (bity12, constraints)
              Nothing ->
                pure (bity12, constraints)
          _ -> do
            let Expr (_, ann1) _ = e1
            spanInFile1 <- askSpanInFile ann1
            analysisError $ NotAFunction spanInFile1 bity1
      pure (Expr (bt, ann) (App e1' e2'), bity, constraints)
    LetIn x e1 e2 -> do
      -- Not confident. TODO: check the validity of the following
      (e1', bity1@(BIType (bt1, _) _), constraints1) <- extractConstraintsFromExpr btenv e1
      (e2', bity2@(BIType (bt2, _) _), constraints2) <-
        extractConstraintsFromExpr (Map.insert x (EntryLocallyBound bt bity1) btenv) e2
      let e' = Expr (bt, ann) (LetIn x e1' e2')
      pure (e', bity2, constraints1 ++ constraints2 ++ [CLeq ann bt bt1, CLeq ann bt bt2])

extractConstraintsFromTypeExpr :: BindingTimeEnv -> BTypeExpr -> M (BTypeExpr, BIType, [Constraint Span])
extractConstraintsFromTypeExpr btenv (TypeExpr (bt, ann) typeExprMain) =
  case typeExprMain of
    TyName tyName args -> do
      (args', constraints) <-
        case args of
          [] ->
            pure ([], [])
          _ : _ -> do
            triples <- mapM (extractConstraintsFromExpr btenv) args
            let args' = map (\(e', _, _) -> e') triples
            let bts = map (\(_, BIType (bt', _) _, _) -> bt') triples
            let constraintsSub = concatMap (\(_, _, constraints') -> constraints') triples
            let constraintsArgsZero = map (\bt' -> CEqual ann bt' (BTConst BT0)) bts
            let constraints = constraintsSub ++ constraintsArgsZero ++ [CEqual ann bt (BTConst BT1)]
            pure (args', constraints)
      let tye' = TypeExpr (bt, ann) (TyName tyName args')
      pure (tye', BIType (bt, ann) (BITyBase args'), constraints)
    TyArrow (x1opt, tye1) tye2 -> do
      (tye1', bity1@(BIType (bt1, _) _), constraints1) <- extractConstraintsFromTypeExpr btenv tye1
      case x1opt of
        Nothing -> do
          (tye2', bity2@(BIType (bt2, _) _), constraints2) <- extractConstraintsFromTypeExpr btenv tye2
          let constraints = [CLeq ann bt bt1, CLeq ann bt bt2]
          let tye' = TypeExpr (bt, ann) (TyArrow (Nothing, tye1') tye2')
          pure (tye', BIType (bt, ann) (BITyArrow (Nothing, bity1) bity2), constraints1 ++ constraints2 ++ constraints)
        Just x1 -> do
          (tye2', bity2@(BIType (bt2, _) _), constraints2) <-
            extractConstraintsFromTypeExpr (Map.insert x1 (EntryLocallyBound bt bity1) btenv) tye2
          let constraints =
                if occurs x1 bity2
                  then [CEqual ann bt (BTConst BT0)]
                  else [CLeq ann bt bt1, CLeq ann bt bt2]
          let tye' = TypeExpr (bt, ann) (TyArrow (Just x1, tye1') tye2')
          pure (tye', BIType (bt, ann) (BITyArrow (x1opt, bity1) bity2), constraints1 ++ constraints2 ++ constraints)

stage :: Bool -> BindingTimeEnv -> Expr -> M (BCExprF Span, Lwsd.Expr)
stage fallBackToBindingTime0 btenv e = do
  let be = evalState (assignBindingTimeVarToExpr e) initialState
  (be', _bity, constraints) <- extractConstraintsFromExpr btenv be
  (rawSolutionMap, _unsolvedConstraints) <-
    case solveConstraints constraints of
      Left ann -> do
        spanInFile <- askSpanInFile ann
        analysisError $ BindingTimeContradiction spanInFile
      Right pair ->
        pure pair
  let solutionMap = Map.mapMaybe (^? #_BTConst) rawSolutionMap
  let btcFallback = if fallBackToBindingTime0 then BT0 else BT1
  let bce =
        fmap
          ( \(bt, ann) ->
              case bt of
                BTConst btc ->
                  (btc, ann)
                BTVar btv ->
                  case Map.lookup btv solutionMap of
                    Just btc -> (btc, ann)
                    Nothing -> (btcFallback, ann)
          )
          be'
  let lwe = stageExpr0 bce
  pure (bce, lwe)

run :: SourceSpec -> Bool -> BindingTimeEnv -> Expr -> Either AnalysisError (BCExprF Span, Lwsd.Expr)
run sourceSpec fallBackToBindingTime0 btenv e =
  runReaderT (stage fallBackToBindingTime0 btenv e) analysisConfig
  where
    analysisConfig = AnalysisConfig {sourceSpec}
