module Surface.BindingTimeAnalyzer
  ( BindingTimeVar,
    BExpr,
    BITypeF (..),
    BITypeMainF (..),
    BIType,
    BCExpr,
    BCExprMain,
    BCTypeExpr,
    BCTypeExprMain,
    AnalysisError (..),
    BindingTimeEnvEntry (..),
    BindingTimeEnv,
    BindingTimeConst (..),
    BindingTime (..),
    run,
  )
where

import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Generics.Labels ()
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics
import Lwsd.Syntax qualified as Lwsd
import Surface.Syntax
import Util.LocationInFile (SourceSpec, SpanInFile, getSpanInFile)
import Util.TokenUtil (Span)
import Prelude hiding (succ)

newtype BindingTimeVar = BindingTimeVar Int
  deriving stock (Ord, Eq, Show)

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

type BExpr = ExprF (BindingTime, Span)

type BTypeExpr = TypeExprF (BindingTime, Span)

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

data BindingTimeConst = BT0 | BT1
  deriving stock (Eq, Ord, Show) -- BT0 < BT1

data BindingTime
  = BTConst BindingTimeConst
  | BTVar BindingTimeVar
  deriving stock (Show, Generic)

data Constraint
  = CLeq Span BindingTime BindingTime
  | CEqual Span BindingTime BindingTime
  deriving stock (Show)

-- Intermediate, minimal type representations for binding-time analysis
data BITypeF meta = BIType meta (BITypeMainF meta)
  deriving stock (Show)

data BITypeMainF meta
  = BITyBase [ExprF meta]
  | BITyArrow (Maybe Var, BITypeF meta) (BITypeF meta)
  deriving stock (Show)

type BIType = BITypeF (BindingTime, Span)

data BindingTimeEnvEntry
  = EntryBuiltInPersistent (BITypeF ())
  | EntryBuiltInFixed Var BindingTimeConst (BITypeF BindingTimeConst)
  | EntryLocallyBound BindingTime BIType

type BindingTimeEnv = Map Var BindingTimeEnvEntry

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

class HasVar a where
  frees :: a -> Set Var
  subst :: BExpr -> Var -> a -> a

instance HasVar BIType where
  frees (BIType _meta bityMain) =
    case bityMain of
      BITyBase bes -> Set.unions (map frees bes)
      BITyArrow (Nothing, btye1) btye2 -> Set.union (frees btye1) (frees btye2)
      BITyArrow (Just x1, btye1) btye2 -> Set.union (frees btye1) (Set.delete x1 (frees btye2))
  subst be0 x (BIType meta bityMain) =
    BIType meta $
      case bityMain of
        BITyBase bes -> BITyBase (map f bes)
        BITyArrow (Nothing, btye1) btye2 -> BITyArrow (Nothing, f btye1) (f btye2)
        BITyArrow (Just y, btye1) btye2 -> BITyArrow (Just y, f btye1) (if y == x then btye2 else f btye2)
    where
      f :: forall a. (HasVar a) => a -> a
      f = subst be0 x

instance HasVar BExpr where
  frees (Expr _ann exprMain) =
    case exprMain of
      Literal _ -> Set.empty
      Var x -> Set.singleton x
      Lam (x, tye1) e2 -> Set.union (frees tye1) (Set.delete x (frees e2))
      App e1 e2 -> Set.union (frees e1) (frees e2)
      LetIn x e1 e2 -> Set.union (frees e1) (Set.delete x (frees e2))
  subst be0 x be@(Expr ann exprMain) =
    case exprMain of
      Literal _ -> be
      Var y -> if y == x then be0 else be
      Lam (y, tye1) e2 -> Expr ann (Lam (y, f tye1) (if y == x then e2 else f e2))
      App e1 e2 -> Expr ann (App (f e1) (f e2))
      LetIn y e1 e2 -> Expr ann (LetIn y (f e1) (if y == x then e2 else f e2))
    where
      f :: forall a. (HasVar a) => a -> a
      f = subst be0 x

instance HasVar BTypeExpr where
  frees (TypeExpr _ann typeExprMain) =
    case typeExprMain of
      TyName _tyName args -> Set.unions (map frees args)
      TyArrow (Nothing, tye1) tye2 -> Set.union (frees tye1) (frees tye2)
      TyArrow (Just y, tye1) tye2 -> Set.union (frees tye1) (Set.delete y (frees tye2))
  subst be0 x (TypeExpr ann typeExprMain) =
    TypeExpr ann $
      case typeExprMain of
        TyName tyName args -> TyName tyName (map f args)
        TyArrow (Nothing, btye1) btye2 -> TyArrow (Nothing, f btye1) (f btye2)
        TyArrow (Just y, btye1) btye2 -> TyArrow (Just y, f btye1) (if y == x then btye2 else f btye2)
    where
      f :: forall a. (HasVar a) => a -> a
      f = subst be0 x

occurs :: (HasVar a) => Var -> a -> Bool
occurs x entity = x `elem` frees entity

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

extractConstraintsFromExpr :: BindingTimeEnv -> BExpr -> M (BExpr, BIType, [Constraint])
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

extractConstraintsFromTypeExpr :: BindingTimeEnv -> BTypeExpr -> M (BTypeExpr, BIType, [Constraint])
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

data SolvingStepResult
  = NotFound
  | Subst BindingTimeVar BindingTime
  | TrivialEliminated [Constraint]
  | ContradictionDetected Span

type BindingTimeSubst = Map BindingTimeVar BindingTime

solveConstraints :: [Constraint] -> M (BindingTimeSubst, [Constraint])
solveConstraints = go Map.empty
  where
    go :: BindingTimeSubst -> [Constraint] -> M (BindingTimeSubst, [Constraint])
    go accMap constraints =
      case step [] constraints of
        NotFound ->
          pure (accMap, constraints)
        Subst btvFrom btTo -> do
          let accMapNew = Map.insert btvFrom btTo (substSubst btvFrom btTo accMap)
          let constraintsNew = map (substConstraint btvFrom btTo) constraints
          go accMapNew constraintsNew
        TrivialEliminated constraintsNew ->
          go accMap constraintsNew
        ContradictionDetected ann -> do
          spanInFile <- askSpanInFile ann
          analysisError $ BindingTimeContradiction spanInFile

    step :: [Constraint] -> [Constraint] -> SolvingStepResult
    step constraintAcc = \case
      [] -> NotFound
      CLeq _ann (BTVar btv1) (BTVar btv2) : rest | btv1 == btv2 -> TrivialEliminated (reverse constraintAcc ++ rest)
      CLeq _ann (BTVar btv1) (BTConst BT0) : _ -> Subst btv1 (BTConst BT0)
      CLeq _ann (BTConst BT1) (BTVar btv2) : _ -> Subst btv2 (BTConst BT1)
      CLeq _ann (BTConst BT0) _ : rest -> TrivialEliminated (reverse constraintAcc ++ rest)
      CLeq _ann _ (BTConst BT1) : rest -> TrivialEliminated (reverse constraintAcc ++ rest)
      CLeq ann (BTConst BT1) (BTConst BT0) : _ -> ContradictionDetected ann
      CEqual _ann (BTVar btv1) (BTVar btv2) : rest | btv1 == btv2 -> TrivialEliminated (reverse constraintAcc ++ rest)
      CEqual _ann (BTVar btv1) bt2 : _ -> Subst btv1 bt2
      CEqual _ann bt1 (BTVar btv2) : _ -> Subst btv2 bt1
      CEqual ann (BTConst btc1) (BTConst btc2) : rest ->
        if btc1 == btc2
          then TrivialEliminated (reverse constraintAcc ++ rest)
          else ContradictionDetected ann
      constraint : rest -> step (constraint : constraintAcc) rest

    substSubst :: BindingTimeVar -> BindingTime -> BindingTimeSubst -> BindingTimeSubst
    substSubst btvFrom btTo =
      Map.map (substBindingTime btvFrom btTo)

    substConstraint :: BindingTimeVar -> BindingTime -> Constraint -> Constraint
    substConstraint btvFrom btTo = \case
      CLeq ann bt1 bt2 -> CLeq ann (f bt1) (f bt2)
      CEqual ann bt1 bt2 -> CEqual ann (f bt1) (f bt2)
      where
        f = substBindingTime btvFrom btTo

    substBindingTime :: BindingTimeVar -> BindingTime -> BindingTime -> BindingTime
    substBindingTime btvFrom btTo bt =
      case bt of
        BTVar btv -> if btv == btvFrom then btTo else bt
        BTConst _ -> bt

type BCExpr = ExprF (BindingTimeConst, Span)

type BCExprMain = ExprMainF (BindingTimeConst, Span)

type BCTypeExpr = TypeExprF (BindingTimeConst, Span)

type BCTypeExprMain = TypeExprMainF (BindingTimeConst, Span)

stageExpr0 :: BCExpr -> Lwsd.Expr
stageExpr0 (Expr (btc, ann) exprMain) =
  case btc of
    BT0 -> Lwsd.Expr ann (stageExpr0Main exprMain)
    BT1 -> Lwsd.Expr ann (Lwsd.Bracket (Lwsd.Expr ann (stageExpr1Main exprMain)))

stageExpr0Main :: BCExprMain -> Lwsd.ExprMain
stageExpr0Main = \case
  Literal lit -> Lwsd.Literal (convertLiteral lit)
  Var x -> Lwsd.Var x
  Lam (x, tye1) e2 -> Lwsd.Lam (x, stageTypeExpr0 tye1) (stageExpr0 e2)
  App e1 e2 -> Lwsd.App (stageExpr0 e1) (stageExpr0 e2)
  LetIn x e1 e2 -> Lwsd.LetIn x (stageExpr0 e1) (stageExpr0 e2)

stageExpr1 :: BCExpr -> Lwsd.Expr
stageExpr1 (Expr (btc, ann) exprMain) =
  case btc of
    BT0 -> Lwsd.Expr ann (Lwsd.Escape (Lwsd.Expr ann (stageExpr0Main exprMain)))
    BT1 -> Lwsd.Expr ann (stageExpr1Main exprMain)

stageExpr1Main :: BCExprMain -> Lwsd.ExprMain
stageExpr1Main = \case
  Literal lit -> Lwsd.Literal (convertLiteral lit)
  Var x -> Lwsd.Var x
  Lam (x, tye1) e2 -> Lwsd.Lam (x, stageTypeExpr1 tye1) (stageExpr1 e2)
  App e1 e2 -> Lwsd.App (stageExpr1 e1) (stageExpr1 e2)
  LetIn x e1 e2 -> Lwsd.LetIn x (stageExpr1 e1) (stageExpr1 e2)

stageTypeExpr0 :: BCTypeExpr -> Lwsd.TypeExpr
stageTypeExpr0 (TypeExpr (btc, ann) typeExprMain) =
  case btc of
    BT1 -> Lwsd.TypeExpr ann (Lwsd.TyCode (Lwsd.TypeExpr ann (stageTypeExpr1Main typeExprMain)))
    BT0 -> Lwsd.TypeExpr ann (stageTypeExpr0Main typeExprMain)

stageTypeExpr0Main :: BCTypeExprMain -> Lwsd.TypeExprMain
stageTypeExpr0Main = \case
  TyName tyName args ->
    case args of
      [] -> Lwsd.TyName tyName []
      _ : _ -> error "stageTypeExpr0Main, non-empty `args`"
  TyArrow (xOpt, tye1) tye2 ->
    Lwsd.TyArrow (xOpt, stageTypeExpr0 tye1) (stageTypeExpr0 tye2)

stageTypeExpr1 :: BCTypeExpr -> Lwsd.TypeExpr
stageTypeExpr1 (TypeExpr (btc, ann) typeExprMain) =
  case btc of
    BT0 -> error "stageTypeExpr1, BT0"
    BT1 -> Lwsd.TypeExpr ann (stageTypeExpr1Main typeExprMain)

stageTypeExpr1Main :: BCTypeExprMain -> Lwsd.TypeExprMain
stageTypeExpr1Main = \case
  TyName tyName args -> Lwsd.TyName tyName (map (Lwsd.PersistentArg . stageExpr0) args)
  TyArrow (xOpt, tye1) tye2 -> Lwsd.TyArrow (xOpt, stageTypeExpr1 tye1) (stageTypeExpr1 tye2)

convertLiteral :: Literal -> Lwsd.Literal
convertLiteral = \case
  LitInt n -> Lwsd.LitInt n
  LitVec ns -> Lwsd.LitVec ns
  LitMat nss -> Lwsd.LitMat nss

stage :: Bool -> BindingTimeEnv -> Expr -> M (BCExpr, Lwsd.Expr)
stage fallBackToBindingTime0 btenv e = do
  let be = evalState (assignBindingTimeVarToExpr e) initialState
  (be', _bity, constraints) <- extractConstraintsFromExpr btenv be
  (rawSolutionMap, _unsolvedConstraints) <- solveConstraints constraints
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

run :: SourceSpec -> Bool -> BindingTimeEnv -> Expr -> Either AnalysisError (BCExpr, Lwsd.Expr)
run sourceSpec fallBackToBindingTime0 btenv e =
  runReaderT (stage fallBackToBindingTime0 btenv e) analysisConfig
  where
    analysisConfig = AnalysisConfig {sourceSpec}
