module Surface.BindingTimeAnalyzer
  ( BindingTimeVar,
    initialState,
    assignBindingTimeVarToExpr,
    BindingTimeConst (..),
    BindingTime (..),
    Constraint (..),
    extractConstraintsFromExpr,
  )
where

import Control.Monad.Trans.State
import Data.Either.Extra qualified as Either
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Lwsd.Matrix qualified as Matrix
import Lwsd.Vector qualified as Vector
import Surface.Syntax
import Util.TokenUtil (Span)
import Prelude hiding (succ)

newtype BindingTimeVar = BindingTimeVar Int
  deriving stock (Show)

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

data BindingTime
  = BTConst BindingTimeConst
  | BTVar BindingTimeVar

data Constraint
  = CLeq BindingTimeVar BindingTime
  | CEqual BindingTimeVar BindingTime

-- Intermediate type representations
data BIType = BIType (BindingTimeVar, Span) BITypeMain

data BITypeMain
  = BITyInt
  | BITyVecLit Int
  | BITyVecExpr BExpr
  | BITyMatLit (Int, Int)
  | BITyMatExpr (BExpr, BExpr)
  | BITyArrow (Maybe Var, BIType) BIType

data BindingTimeError
  = InvalidMatrixLiteral Span Matrix.ConstructionError
  | UnboundVar Span Var
  | NotAFunction BIType
  | UnknownTypeOrInvalidArity Span TypeName Int

type BindingTimeEnv = Map Var (BindingTimeVar, BIType)

type M a = Either BindingTimeError a

analysisError :: BindingTimeError -> M a
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
        Just (btv', bity) ->
          pure (bity, [CEqual btv (BTVar btv')])
    Lam (x1, btye1) e2 -> do
      (bity1@(BIType (btv1, _) _), constraints1) <- extractConstraintsFromTypeExpr btenv btye1
      (bity2@(BIType (btv2, _) _), constraints2) <- extractConstraintsFromExpr (Map.insert x1 (btv, bity1) btenv) e2
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
    LetIn _x _e1 _e2 -> do
      error "TODO: LetIn"

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
            extractConstraintsFromTypeExpr (Map.insert x1 (btv, bity1) btenv) tye2
          let constraints =
                if occurs x1 bity2
                  then [CEqual btv (BTConst BT0)]
                  else [CLeq btv (BTVar btv1), CLeq btv (BTVar btv2)]
          pure (BIType (btv, ann) (BITyArrow (x1opt, bity1) bity2), constraints1 ++ constraints2 ++ constraints)
