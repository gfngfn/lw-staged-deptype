module Surface.BindingTimeAnalyzer
  ( BindingTimeVar,
    BExpr,
    BITypeF (..),
    BITypeMainF (..),
    BIType,
    BITypeVoid,
    ExprVoid,
    TypeExprVoid,
    BindingTimeEnvEntry (..),
    BindingTimeEnv,
    initialState,
    assignBindingTimeVarToExpr,
    BindingTimeConst (..),
    BindingTime (..),
    Constraint (..),
    extractConstraintsFromExpr,
    run,
  )
where

import Control.Lens
import Control.Monad.Trans.State
import Data.Either.Extra qualified as Either
import Data.Generics.Labels ()
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics
import Lwsd.Matrix qualified as Matrix
import Lwsd.Syntax qualified as Lwsd
import Lwsd.Vector qualified as Vector
import Surface.Syntax
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

type BExpr = ExprF (BindingTimeVar, Span)

type BTypeExpr = TypeExprF (BindingTimeVar, Span)

assignBindingTimeVarToExpr :: Expr -> Assigner BExpr
assignBindingTimeVarToExpr (Expr ann exprMain) = do
  btv <- fresh
  Expr (btv, ann)
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
  TypeExpr (btv, ann)
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
  = CLeq BindingTimeVar BindingTime
  | CEqual BindingTimeVar BindingTime
  deriving stock (Show)

-- Intermediate type representations
data BITypeF meta = BIType meta (BITypeMainF meta)
  deriving stock (Show)

data BITypeMainF meta
  = BITyInt
  | BITyVecLit Int
  | BITyVecExpr (ExprF meta)
  | BITyMatLit (Int, Int)
  | BITyMatExpr (ExprF meta, ExprF meta)
  | BITyArrow (Maybe Var, BITypeF meta) (BITypeF meta)
  deriving stock (Show)

type BIType = BITypeF (BindingTimeVar, Span)

type BITypeVoid = BITypeF ()

type ExprVoid = ExprF ()

type TypeExprVoid = TypeExprF ()

data AnalysisError
  = InvalidMatrixLiteral Span Matrix.ConstructionError
  | UnboundVar Span Var
  | NotAFunction BIType
  | UnknownTypeOrInvalidArity Span TypeName Int
  | BindingTimeContradiction
  deriving stock (Show)

data BindingTimeEnvEntry
  = EntryBuiltIn (BITypeF ())
  | EntryLocallyBound BindingTimeVar BIType

type BindingTimeEnv = Map Var BindingTimeEnvEntry

type M a = Either AnalysisError a

analysisError :: AnalysisError -> M a
analysisError = Left

class HasVar a where
  frees :: a -> Set Var
  subst :: BExpr -> Var -> a -> a

instance HasVar BIType where
  frees (BIType _meta bityMain) =
    case bityMain of
      BITyInt -> Set.empty
      BITyVecLit _ -> Set.empty
      BITyVecExpr be -> frees be
      BITyMatLit _ -> Set.empty
      BITyMatExpr (be1, be2) -> Set.union (frees be1) (frees be2)
      BITyArrow (Nothing, btye1) btye2 -> Set.union (frees btye1) (frees btye2)
      BITyArrow (Just x1, btye1) btye2 -> Set.union (frees btye1) (Set.delete x1 (frees btye2))
  subst be0 x (BIType meta bityMain) =
    BIType meta $
      case bityMain of
        BITyInt -> bityMain
        BITyVecLit _ -> bityMain
        BITyVecExpr be -> BITyVecExpr (f be)
        BITyMatLit _ -> bityMain
        BITyMatExpr (be1, be2) -> BITyMatExpr (f be1, f be2)
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

enhanceBIType :: BindingTimeVar -> Span -> BITypeVoid -> BIType
enhanceBIType btv ann (BIType () bityMain) =
  BIType (btv, ann) $
    case bityMain of
      BITyInt -> BITyInt
      BITyVecLit n -> BITyVecLit n
      BITyVecExpr e -> BITyVecExpr (fExpr e)
      BITyMatLit nPair -> BITyMatLit nPair
      BITyMatExpr (e1, e2) -> BITyMatExpr (fExpr e1, fExpr e2)
      BITyArrow (xOpt, bity1) bity2 -> BITyArrow (xOpt, fBIType bity1) (fBIType bity2)
  where
    fBIType = enhanceBIType btv ann
    fExpr = enhanceExpr btv ann

enhanceExpr :: BindingTimeVar -> Span -> ExprVoid -> BExpr
enhanceExpr btv ann (Expr () exprMain) =
  Expr (btv, ann) $
    case exprMain of
      Literal lit -> Literal lit
      Var x -> Var x
      Lam (x, tye1) e2 -> Lam (x, fTypeExpr tye1) (fExpr e2)
      App e1 e2 -> App (fExpr e1) (fExpr e2)
      LetIn x e1 e2 -> LetIn x (fExpr e1) (fExpr e2)
  where
    fExpr = enhanceExpr btv ann
    fTypeExpr = enhanceTypeExpr btv ann

enhanceTypeExpr :: BindingTimeVar -> Span -> TypeExprVoid -> BTypeExpr
enhanceTypeExpr btv ann (TypeExpr () typeExprMain) =
  TypeExpr (btv, ann) $
    case typeExprMain of
      TyName tyName args -> TyName tyName (map fExpr args)
      TyArrow (xOpt, tye1) tye2 -> TyArrow (xOpt, fTypeExpr tye1) (fTypeExpr tye2)
  where
    fExpr = enhanceExpr btv ann
    fTypeExpr = enhanceTypeExpr btv ann

extractConstraintsFromExpr :: BindingTimeEnv -> BExpr -> M (BIType, [Constraint])
extractConstraintsFromExpr btenv (Expr (btv, ann) exprMain) =
  case exprMain of
    Literal lit -> do
      bityMain <-
        case lit of
          LitInt _ ->
            pure $ BITyInt
          LitVec ns -> do
            let vec = Vector.fromList ns
            pure $ BITyVecLit (Vector.length vec)
          LitMat nss -> do
            mat <- Either.mapLeft (InvalidMatrixLiteral ann) $ Matrix.fromRows nss
            pure $ BITyMatLit (Matrix.size mat)
      pure (BIType (btv, ann) bityMain, [])
    Var x ->
      case Map.lookup x btenv of
        Nothing ->
          analysisError $ UnboundVar ann x
        Just (EntryBuiltIn bityVoid) ->
          pure (enhanceBIType btv ann bityVoid, []) -- TODO: remove `ann`
        Just (EntryLocallyBound btv' bity) ->
          pure (bity, [CEqual btv (BTVar btv')])
    Lam (x1, btye1) e2 -> do
      (bity1@(BIType (btv1, _) _), constraints1) <- extractConstraintsFromTypeExpr btenv btye1
      (bity2@(BIType (btv2, _) _), constraints2) <-
        extractConstraintsFromExpr (Map.insert x1 (EntryLocallyBound btv bity1) btenv) e2
      let constraints =
            if occurs x1 bity2
              then [CEqual btv (BTConst BT0)]
              else [CLeq btv (BTVar btv1), CLeq btv (BTVar btv2)]
      pure (BIType (btv, ann) (BITyArrow (Just x1, bity1) bity2), constraints1 ++ constraints2 ++ constraints)
    App e1 e2 -> do
      (bity1@(BIType (btv1, _) bityMain1), constraints1) <- extractConstraintsFromExpr btenv e1
      (_bity2, constraints2) <- extractConstraintsFromExpr btenv e2
      case bityMain1 of
        BITyArrow (x11opt, _bity11) bity12 ->
          -- We could check here that `bity2` and `bity11` are compatible,
          -- but it can be deferred to the upcoming type-checking.
          case x11opt of
            Just x11 ->
              if occurs x11 bity12
                then pure (subst e2 x11 bity12, constraints1 ++ constraints2 ++ [CEqual btv (BTConst BT0)])
                else pure (bity12, constraints1 ++ constraints2 ++ [CEqual btv (BTVar btv1)])
            Nothing ->
              pure (bity12, constraints1 ++ constraints2 ++ [CEqual btv (BTVar btv1)])
        _ ->
          analysisError $ NotAFunction bity1
    LetIn x e1 e2 -> do
      -- Not confident. TODO: check the validity of the following
      (bity1@(BIType (btv1, _) _), constraints1) <- extractConstraintsFromExpr btenv e1
      (bity2@(BIType (btv2, _) _), constraints2) <-
        extractConstraintsFromExpr (Map.insert x (EntryLocallyBound btv bity1) btenv) e2
      pure (bity2, constraints1 ++ constraints2 ++ [CLeq btv (BTVar btv1), CLeq btv (BTVar btv2)])

extractConstraintsFromTypeExpr :: BindingTimeEnv -> BTypeExpr -> M (BIType, [Constraint])
extractConstraintsFromTypeExpr btenv (TypeExpr (btv, ann) typeExprMain) =
  case typeExprMain of
    TyName tyName args -> do
      (bityMain, constraints) <-
        case (tyName, args) of
          ("Int", []) ->
            pure (BITyInt, [])
          ("Vec", [e1]) -> do
            (BIType (btv1, _) _bity1Main, constraints1) <- extractConstraintsFromExpr btenv e1
            -- We could check here that `bity1Main` equals `Int`,
            -- but it can be deferred to the upcoming type-checking.
            pure (BITyVecExpr e1, constraints1 ++ [CEqual btv1 (BTConst BT0), CEqual btv (BTConst BT1)])
          ("Mat", [e1, e2]) -> do
            (BIType (btv1, _) _bity1Main, constraints1) <- extractConstraintsFromExpr btenv e1
            (BIType (btv2, _) _bity2Main, constraints2) <- extractConstraintsFromExpr btenv e2
            -- We could check here that both `bity1Main` and `bity2Main` equal `Int`,
            -- but it can be deferred to the upcoming type-checking.
            let constraints = [CEqual btv1 (BTConst BT0), CEqual btv2 (BTConst BT0), CEqual btv (BTConst BT1)]
            pure (BITyMatExpr (e1, e2), constraints1 ++ constraints2 ++ constraints)
          _ ->
            analysisError $ UnknownTypeOrInvalidArity ann tyName (length args)
      pure (BIType (btv, ann) bityMain, constraints)
    TyArrow (x1opt, tye1) tye2 -> do
      (bity1@(BIType (btv1, _) _), constraints1) <- extractConstraintsFromTypeExpr btenv tye1
      case x1opt of
        Nothing -> do
          (bity2@(BIType (btv2, _) _), constraints2) <- extractConstraintsFromTypeExpr btenv tye2
          let constraints = [CLeq btv (BTVar btv1), CLeq btv (BTVar btv2)]
          pure (BIType (btv, ann) (BITyArrow (Nothing, bity1) bity2), constraints1 ++ constraints2 ++ constraints)
        Just x1 -> do
          (bity2@(BIType (btv2, _) _), constraints2) <-
            extractConstraintsFromTypeExpr (Map.insert x1 (EntryLocallyBound btv bity1) btenv) tye2
          let constraints =
                if occurs x1 bity2
                  then [CEqual btv (BTConst BT0)]
                  else [CLeq btv (BTVar btv1), CLeq btv (BTVar btv2)]
          pure (BIType (btv, ann) (BITyArrow (x1opt, bity1) bity2), constraints1 ++ constraints2 ++ constraints)

data SolvingStepResult
  = NotFound
  | Subst BindingTimeVar BindingTime
  | TrivialEliminated [Constraint]

type BindingTimeSubst = Map BindingTimeVar BindingTime

solveConstraints :: [Constraint] -> Either AnalysisError (BindingTimeSubst, [Constraint])
solveConstraints = go Map.empty
  where
    go :: BindingTimeSubst -> [Constraint] -> Either AnalysisError (BindingTimeSubst, [Constraint])
    go accMap constraints =
      case step [] constraints of
        NotFound ->
          pure (accMap, constraints)
        Subst btv1 bt2 -> do
          let accMapNew = Map.insert btv1 bt2 (substSubst btv1 bt2 accMap)
          constraintssNew <- mapM (substConstraint btv1 bt2) constraints
          go accMapNew (concat constraintssNew)
        TrivialEliminated constraintsNew ->
          go accMap constraintsNew

    step :: [Constraint] -> [Constraint] -> SolvingStepResult
    step constraintAcc = \case
      [] -> NotFound
      CLeq btv1 (BTConst BT0) : _ -> Subst btv1 (BTConst BT0)
      CLeq _ (BTConst BT1) : rest -> TrivialEliminated (reverse constraintAcc ++ rest)
      CLeq btv1 (BTVar btv2) : rest | btv1 == btv2 -> TrivialEliminated (reverse constraintAcc ++ rest)
      CEqual btv1 (BTVar btv2) : rest | btv1 == btv2 -> TrivialEliminated (reverse constraintAcc ++ rest)
      CEqual btv1 bt2 : _ -> Subst btv1 bt2
      constraint : rest -> step (constraint : constraintAcc) rest

    substSubst :: BindingTimeVar -> BindingTime -> BindingTimeSubst -> BindingTimeSubst
    substSubst btv1 bt2 =
      Map.map
        ( \case
            BTConst btc -> BTConst btc
            BTVar btv -> if btv == btv1 then bt2 else BTVar btv
        )

    substConstraint :: BindingTimeVar -> BindingTime -> Constraint -> Either AnalysisError [Constraint]
    substConstraint btv1 bt2 constraint =
      case bt2 of
        BTVar btv2 ->
          case constraint of
            CLeq btvC1 (BTVar btvC2) ->
              if btvC1 == btv1
                then
                  pure [CLeq btv2 (BTVar btvC2)]
                else
                  if btvC2 == btv1
                    then
                      pure [CLeq btvC1 (BTVar btv2)]
                    else
                      pure [constraint]
            CLeq btvC1 (BTConst btcC2) ->
              if btvC1 == btv1
                then
                  pure [CLeq btv2 (BTConst btcC2)]
                else
                  pure [constraint]
            CEqual btvC1 (BTVar btvC2) ->
              if btvC1 == btv1
                then
                  pure [CEqual btv2 (BTVar btvC2)]
                else
                  if btvC2 == btv1
                    then
                      pure [CEqual btvC1 (BTVar btv2)]
                    else
                      pure [constraint]
            CEqual btvC1 (BTConst btcC2) ->
              if btvC1 == btv1
                then
                  pure [CEqual btv2 (BTConst btcC2)]
                else
                  pure [constraint]
        BTConst btc2 ->
          case constraint of
            CLeq btvC1 (BTVar btvC2) ->
              if btvC1 == btv1
                then case btc2 of
                  BT0 -> pure []
                  BT1 -> pure [CEqual btvC2 (BTConst BT1)]
                else
                  if btvC2 == btv1
                    then
                      pure [CLeq btvC1 (BTConst btc2)]
                    else
                      pure [constraint]
            CLeq btvC1 (BTConst btcC2) ->
              if btvC1 == btv1
                then
                  if btc2 <= btcC2
                    then
                      pure []
                    else
                      Left BindingTimeContradiction
                else
                  pure [constraint]
            CEqual btvC1 (BTVar btvC2) ->
              if btvC1 == btv1
                then
                  pure [CEqual btvC2 (BTConst btc2)]
                else
                  pure [constraint]
            CEqual btvC1 (BTConst btcC2) ->
              if btvC1 == btv1
                then
                  if btc2 == btcC2
                    then
                      pure []
                    else
                      Left BindingTimeContradiction
                else
                  pure [constraint]

type BCExpr = ExprF (BindingTimeConst, Span)

type BCExprMain = ExprMainF (BindingTimeConst, Span)

type BCTypeExpr = TypeExprF (BindingTimeConst, Span)

type BCTypeExprMain = TypeExprMainF (BindingTimeConst, Span)

stageExpr0 :: BCExpr -> Lwsd.Expr
stageExpr0 (Expr (btc, ann) exprMain) =
  case btc of
    BT1 ->
      let lweMain = stageExpr1Main exprMain
       in Lwsd.Expr ann (Lwsd.Bracket (Lwsd.Expr ann lweMain))
    BT0 ->
      Lwsd.Expr ann (stageExpr0Main exprMain)

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
    BT0 ->
      let lweMain = stageExpr0Main exprMain
       in Lwsd.Expr ann (Lwsd.Escape (Lwsd.Expr ann lweMain))
    BT1 ->
      Lwsd.Expr ann (stageExpr1Main exprMain)

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
    BT1 ->
      let lwtyeMain = stageTypeExpr1Main typeExprMain
       in Lwsd.TypeExpr ann (Lwsd.TyCode (Lwsd.TypeExpr ann lwtyeMain))
    BT0 ->
      Lwsd.TypeExpr ann (stageTypeExpr0Main typeExprMain)

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

run :: BindingTimeEnv -> Expr -> Either AnalysisError (BExpr, Lwsd.Expr)
run btenv e = do
  let be = evalState (assignBindingTimeVarToExpr e) initialState
  (_bity, constraints) <- extractConstraintsFromExpr btenv be
  (rawSolutionMap, _unsolvedConstraints) <- solveConstraints constraints
  let solutionMap = Map.mapMaybe (^? #_BTConst) rawSolutionMap
  let bce =
        fmap
          ( \(btv, ann) ->
              case Map.lookup btv solutionMap of
                Just btc -> (btc, ann)
                Nothing -> (BT1, ann) -- TODO: reconsider this
          )
          be
  let lwe = stageExpr0 bce
  pure (be, lwe)
