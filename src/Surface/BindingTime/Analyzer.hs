module Surface.BindingTime.Analyzer
  ( AnalysisError (..),
    run,
  )
where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Map qualified as Map
import Surface.BindingTime.AnalysisError
import Surface.BindingTime.Constraint
import Surface.BindingTime.Core
import Surface.Syntax
import Util.LocationInFile (SourceSpec, SpanInFile, getSpanInFile)
import Util.TokenUtil
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

-- TODO: merge this function into `extractConstraintsFromExpr`
assignBindingTimeVarToExpr :: Expr -> Assigner BExpr
assignBindingTimeVarToExpr (Expr ann exprMain) = do
  btv <- fresh
  Expr (BTVar btv, ann)
    <$> case exprMain of
      Literal lit ->
        pure $ Literal lit
      Var x ->
        pure $ Var x
      Lam Nothing (x, ty) e -> do
        bty <- assignBindingTimeVarToTypeExpr ty
        be <- assignBindingTimeVarToExpr e
        pure $ Lam Nothing (x, bty) be
      Lam (Just (f, tyRec)) (x, ty) e -> do
        btyRec <- assignBindingTimeVarToTypeExpr tyRec
        bty <- assignBindingTimeVarToTypeExpr ty
        be <- assignBindingTimeVarToExpr e
        pure $ Lam (Just (f, btyRec)) (x, bty) be
      App e1 e2 -> do
        be1 <- assignBindingTimeVarToExpr e1
        be2 <- assignBindingTimeVarToExpr e2
        pure $ App be1 be2
      LetIn x e1 e2 -> do
        be1 <- assignBindingTimeVarToExpr e1
        be2 <- assignBindingTimeVarToExpr e2
        pure $ LetIn x be1 be2
      IfThenElse e0 e1 e2 -> do
        be0 <- assignBindingTimeVarToExpr e0
        be1 <- assignBindingTimeVarToExpr e1
        be2 <- assignBindingTimeVarToExpr e2
        pure $ IfThenElse be0 be1 be2
      As e1 tye2 -> do
        be1 <- assignBindingTimeVarToExpr e1
        btye2 <- assignBindingTimeVarToTypeExpr tye2
        pure $ As be1 btye2
      LamOpt (x, ty) e -> do
        bty <- assignBindingTimeVarToTypeExpr ty
        be <- assignBindingTimeVarToExpr e
        pure $ LamOpt (x, bty) be
      AppOptGiven e1 e2 -> do
        be1 <- assignBindingTimeVarToExpr e1
        be2 <- assignBindingTimeVarToExpr e2
        pure $ AppOptGiven be1 be2
      AppOptOmitted e1 -> do
        be1 <- assignBindingTimeVarToExpr e1
        pure $ AppOptOmitted be1

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
      TyOptArrow (x, ty1) ty2 -> do
        bty1 <- assignBindingTimeVarToTypeExpr ty1
        bty2 <- assignBindingTimeVarToTypeExpr ty2
        pure $ TyOptArrow (x, bty1) bty2

newtype AnalysisConfig = AnalysisConfig
  { sourceSpec :: SourceSpec
  }

type M a = ReaderT AnalysisConfig (Either AnalysisError) a

analysisError :: AnalysisError -> M a
analysisError = lift . Left

askSpanInFile :: Span -> M SpanInFile
askSpanInFile loc = do
  AnalysisConfig {sourceSpec} <- ask
  pure $ getSpanInFile sourceSpec loc

enhanceBIType :: (bt -> BindingTime) -> (ann -> (BindingTime, Span)) -> BITypeF bt ann -> BIType
enhanceBIType enhBt enh (BIType bt bityMain) =
  BIType (enhBt bt) $
    case bityMain of
      BITyBase -> BITyBase
      BITyArrow bity1 bity2 -> BITyArrow (fBIType bity1) (fBIType bity2)
      BITyOptArrow bity1 bity2 -> BITyOptArrow (fBIType bity1) (fBIType bity2)
  where
    fBIType = enhanceBIType enhBt enh

extractConstraintsFromExpr :: BindingTimeEnv -> BExpr -> M (BExpr, BIType, [Constraint Span])
extractConstraintsFromExpr btenv (Expr (bt, ann) exprMain) = do
  spanInFile <- askSpanInFile ann
  case exprMain of
    Literal lit -> do
      pure (Expr (bt, ann) (Literal lit), BIType bt BITyBase, [])
    Var x -> do
      (x', bity, constraints) <-
        case Map.lookup x btenv of
          Nothing ->
            analysisError $ UnboundVar spanInFile x
          Just (EntryBuiltInPersistent bityVoid) ->
            -- TODO: refine `ann`
            pure (x, enhanceBIType (\() -> bt) (\() -> (bt, ann)) bityVoid, [])
          Just (EntryBuiltInFixed x' btc' bityConst) ->
            -- TODO: refine `ann`
            pure (x', enhanceBIType BTConst (\btc -> (BTConst btc, ann)) bityConst, [CEqual ann bt (BTConst btc')])
          Just (EntryLocallyBound bt' bity) ->
            pure (x, bity, [CEqual ann bt bt'])
      pure (Expr (bt, ann) (Var x'), bity, constraints)
    Lam Nothing (x1, btye1) e2 -> do
      (btye1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromTypeExpr btenv btye1
      (e2', bity2@(BIType bt2 _), constraints2) <-
        extractConstraintsFromExpr (Map.insert x1 (EntryLocallyBound bt bity1) btenv) e2
      let constraints = [CLeq ann bt bt1, CLeq ann bt bt2]
      let e' = Expr (bt, ann) (Lam Nothing (x1, btye1') e2')
      pure (e', BIType bt (BITyArrow bity1 bity2), constraints1 ++ constraints2 ++ constraints)
    Lam (Just (f, btyeRec)) (x1, btye1) e2 -> do
      -- Not confident. TODO: check the validity of the following
      (btyeRec', bityRec, constraintsRec) <- extractConstraintsFromTypeExpr btenv btyeRec
      (btye1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromTypeExpr btenv btye1
      (e2', bity2@(BIType bt2 _), constraints2) <-
        extractConstraintsFromExpr
          (Map.insert x1 (EntryLocallyBound bt bity1) (Map.insert f (EntryLocallyBound bt bityRec) btenv))
          e2
      let bitySynth = BIType bt (BITyArrow bity1 bity2)
      constraintsEq <- makeConstraintsFromBITypeEquation ann bitySynth bityRec
      let constraints = [CLeq ann bt bt1, CLeq ann bt bt2]
      let e' = Expr (bt, ann) (Lam (Just (f, btyeRec')) (x1, btye1') e2')
      pure (e', bitySynth, constraintsRec ++ constraints1 ++ constraints2 ++ constraintsEq ++ constraints)
    App e1 e2 -> do
      (e1WithoutOpts, bity1WithoutOpts, constraints1) <- extractConstraintsFromExpr btenv e1
      let (e1', bity1@(BIType bt1 bityMain1)) = appendOmittedOptionalArguments e1WithoutOpts bity1WithoutOpts
      (e2', bity2, constraints2) <- extractConstraintsFromExpr btenv e2
      (bity, constraints) <-
        case bityMain1 of
          BITyArrow bity11 bity12 -> do
            let constraints = [CEqual ann bt bt1]
            constraintsEq <- makeConstraintsFromBITypeEquation ann bity2 bity11
            pure (bity12, constraints1 ++ constraints2 ++ constraintsEq ++ constraints)
          _ -> do
            let Expr (_, ann1) _ = e1
            spanInFile1 <- askSpanInFile ann1
            analysisError $ NotAFunction spanInFile1 bity1
      pure (Expr (bt, ann) (App e1' e2'), bity, constraints)
    LetIn x e1 e2 -> do
      -- Not confident. TODO: check the validity of the following
      (e1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromExpr btenv e1
      (e2', bity2@(BIType bt2 _), constraints2) <-
        extractConstraintsFromExpr (Map.insert x (EntryLocallyBound bt bity1) btenv) e2
      let e' = Expr (bt, ann) (LetIn x e1' e2')
      pure (e', bity2, constraints1 ++ constraints2 ++ [CLeq ann bt bt1, CLeq ann bt bt2])
    IfThenElse e0 e1 e2 -> do
      (e0', bity0@(BIType bt0 bityMain0), constraints0) <- extractConstraintsFromExpr btenv e0
      case bityMain0 of
        BITyBase -> do
          (e1', bity1, constraints1) <- extractConstraintsFromExpr btenv e1
          (e2', bity2, constraints2) <- extractConstraintsFromExpr btenv e2
          let e' = Expr (bt, ann) (IfThenElse e0' e1' e2')
          constraintsEq <- makeConstraintsFromBITypeEquation ann bity1 bity2
          pure (e', bity1, constraints0 ++ constraints1 ++ constraints2 ++ constraintsEq ++ [CEqual ann bt bt0])
        _ -> do
          let Expr (_, ann0) _ = e0
          spanInFile0 <- askSpanInFile ann0
          analysisError $ NotABase spanInFile0 bity0
    As e1 btye2 -> do
      -- Not confident. TODO: check the validity of the following
      (e1', BIType bt1 _, constraints1) <- extractConstraintsFromExpr btenv e1
      (btye2', bity2@(BIType bt2 _), constraints2) <- extractConstraintsFromTypeExpr btenv btye2
      pure (Expr (bt, ann) (As e1' btye2'), bity2, constraints1 ++ constraints2 ++ [CEqual ann bt1 bt2])
    LamOpt (x1, btye1) e2 -> do
      (btye1', bity1, constraints1) <- extractConstraintsFromTypeExpr btenv btye1
      (e2', bity2, constraints2) <-
        extractConstraintsFromExpr (Map.insert x1 (EntryLocallyBound bt bity1) btenv) e2
      let constraints = [CEqual ann bt (BTConst BT0)]
      let e' = Expr (bt, ann) (LamOpt (x1, btye1') e2')
      pure (e', BIType bt (BITyOptArrow bity1 bity2), constraints1 ++ constraints2 ++ constraints)
    AppOptGiven e1 e2 -> do
      (e1', bity1@(BIType bt1 bityMain1), constraints1) <- extractConstraintsFromExpr btenv e1
      (e2', bity2, constraints2) <- extractConstraintsFromExpr btenv e2
      (bity, constraints) <-
        case bityMain1 of
          BITyOptArrow bity11 bity12 -> do
            let constraints = [CEqual ann bt bt1]
            constraintsEq <- makeConstraintsFromBITypeEquation ann bity2 bity11
            pure (bity12, constraints1 ++ constraints2 ++ constraintsEq ++ constraints)
          _ -> do
            let Expr (_, ann1) _ = e1
            spanInFile1 <- askSpanInFile ann1
            analysisError $ NotAnOptFunction spanInFile1 bity1
      pure (Expr (bt, ann) (AppOptGiven e1' e2'), bity, constraints)
    AppOptOmitted e1 -> do
      (e1', bity1@(BIType bt1 bityMain1), constraints1) <- extractConstraintsFromExpr btenv e1
      (bity, constraints) <-
        case bityMain1 of
          BITyOptArrow _bity11 bity12 -> do
            let constraints = [CEqual ann bt bt1]
            pure (bity12, constraints1 ++ constraints)
          _ -> do
            let Expr (_, ann1) _ = e1
            spanInFile1 <- askSpanInFile ann1
            analysisError $ NotAnOptFunction spanInFile1 bity1
      pure (Expr (bt, ann) (AppOptOmitted e1'), bity, constraints)

appendOmittedOptionalArguments :: BExpr -> BIType -> (BExpr, BIType)
appendOmittedOptionalArguments e@(Expr (_, ann) _) bity@(BIType _bt bityMain) =
  case bityMain of
    BITyOptArrow _bity1 bity2 ->
      -- TODO: give better location than `ann`
      appendOmittedOptionalArguments (Expr (BTConst BT0, ann) (AppOptOmitted e)) bity2
    _ ->
      (e, bity)

makeConstraintsFromBITypeEquation :: Span -> BIType -> BIType -> M [Constraint Span]
makeConstraintsFromBITypeEquation ann = go
  where
    go :: BIType -> BIType -> M [Constraint Span]
    go bity1@(BIType bt1 bityMain1) bity2@(BIType bt2 bityMain2) =
      ([CEqual ann bt1 bt2] ++)
        <$> case (bityMain1, bityMain2) of
          (BITyBase, BITyBase) ->
            pure []
          (BITyArrow bity11 bity12, BITyArrow bity21 bity22) -> do
            constraints1 <- go bity11 bity21
            constraints2 <- go bity12 bity22
            pure $ constraints1 ++ constraints2
          (BITyOptArrow bity11 bity12, BITyOptArrow bity21 bity22) -> do
            constraints1 <- go bity11 bity21
            constraints2 <- go bity12 bity22
            pure $ constraints1 ++ constraints2
          (_, _) -> do
            spanInFile <- askSpanInFile ann
            analysisError $ BITypeContradiction spanInFile bity1 bity2

extractConstraintsFromTypeExpr :: BindingTimeEnv -> BTypeExpr -> M (BTypeExpr, BIType, [Constraint Span])
extractConstraintsFromTypeExpr btenv (TypeExpr (bt, ann) typeExprMain) =
  case typeExprMain of
    TyName tyName args -> do
      (args', constraints) <-
        case args of
          [] ->
            case tyName of
              "Nat" -> pure ([], [CEqual ann bt (BTConst BT0)])
              _ -> pure ([], [])
          _ : _ -> do
            triples <- mapM (extractConstraintsFromExpr btenv) args
            let args' = map (\(e', _, _) -> e') triples
            let bts = map (\(_, BIType bt' _, _) -> bt') triples
            let constraintsSub = concatMap (\(_, _, constraints') -> constraints') triples
            let constraintsArgsZero = map (\bt' -> CEqual ann bt' (BTConst BT0)) bts
            let constraints = constraintsSub ++ constraintsArgsZero ++ [CEqual ann bt (BTConst BT1)]
            pure (args', constraints)
      let tye' = TypeExpr (bt, ann) (TyName tyName args')
      pure (tye', BIType bt BITyBase, constraints)
    TyArrow (x1opt, tye1) tye2 -> do
      (tye1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromTypeExpr btenv tye1
      case x1opt of
        Nothing -> do
          (tye2', bity2@(BIType bt2 _), constraints2) <- extractConstraintsFromTypeExpr btenv tye2
          let constraints = [CLeq ann bt bt1, CLeq ann bt bt2]
          let tye' = TypeExpr (bt, ann) (TyArrow (Nothing, tye1') tye2')
          pure (tye', BIType bt (BITyArrow bity1 bity2), constraints1 ++ constraints2 ++ constraints)
        Just x1 -> do
          (tye2', bity2@(BIType bt2 _), constraints2) <-
            extractConstraintsFromTypeExpr (Map.insert x1 (EntryLocallyBound bt bity1) btenv) tye2
          let constraints = [CLeq ann bt bt1, CLeq ann bt bt2]
          let tye' = TypeExpr (bt, ann) (TyArrow (Just x1, tye1') tye2')
          pure (tye', BIType bt (BITyArrow bity1 bity2), constraints1 ++ constraints2 ++ constraints)
    TyOptArrow (x1, tye1) tye2 -> do
      (tye1', bity1, constraints1) <- extractConstraintsFromTypeExpr btenv tye1
      (tye2', bity2, constraints2) <-
        extractConstraintsFromTypeExpr (Map.insert x1 (EntryLocallyBound bt bity1) btenv) tye2
      let constraints = [CEqual ann bt (BTConst BT0)]
      let tye' = TypeExpr (bt, ann) (TyOptArrow (x1, tye1') tye2')
      pure (tye', BIType bt (BITyOptArrow bity1 bity2), constraints1 ++ constraints2 ++ constraints)

run :: SourceSpec -> BindingTimeEnv -> Expr -> Either AnalysisError (BExpr, [Constraint Span])
run sourceSpec btenv e = do
  let be = evalState (assignBindingTimeVarToExpr e) initialState
  (be', _bity, constraints) <- runReaderT (extractConstraintsFromExpr btenv be) analysisConfig
  pure (be', constraints)
  where
    analysisConfig = AnalysisConfig {sourceSpec}
