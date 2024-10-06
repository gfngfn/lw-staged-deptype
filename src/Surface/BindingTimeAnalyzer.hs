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
  | BITyMatLit (Int, Int)
  | BITyArrow (Var, BIType) BIType

data BindingTimeError
  = InvalidMatrixLiteral Span Matrix.ConstructionError
  | UnboundVar Span Var
  | NotAFunction BIType

type BindingTimeEnv = Map Var (BindingTimeVar, BIType)

type M a = Either BindingTimeError a

analysisError :: BindingTimeError -> M a
analysisError = Left

occurs :: Var -> BIType -> Bool
occurs = error "TODO: occurs"

subst :: BExpr -> Var -> BIType -> BIType
subst = error "TODO: subst"

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
      pure (BIType (btv, ann) (BITyArrow (x1, bity1) bity2), constraints1 ++ constraints2 ++ constraints)
    App e1 e2 -> do
      (bity1@(BIType (btv1, _) bityMain1), constraints1) <- extractConstraintsFromExpr btenv e1
      (_bity2, constraints2) <- extractConstraintsFromExpr btenv e2
      case bityMain1 of
        BITyArrow (x11, _bity11) bity12 ->
          -- We could check here that `bity2` and `bity11` are compatible,
          -- but it can be deferred to the upcoming type-checking.
          if occurs x11 bity12
            then
              pure (subst e2 x11 bity12, constraints1 ++ constraints2 ++ [CEqual btv (BTConst BT0)])
            else
              pure (bity12, constraints1 ++ constraints2 ++ [CEqual btv (BTVar btv1)])
        _ ->
          analysisError $ NotAFunction bity1
    LetIn _x _e1 _e2 -> do
      error "TODO: LetIn"

extractConstraintsFromTypeExpr :: BindingTimeEnv -> BTypeExpr -> M (BIType, [Constraint])
extractConstraintsFromTypeExpr _btenv _btye =
  error "TODO: extractConstraintsFromTypeExpr"
