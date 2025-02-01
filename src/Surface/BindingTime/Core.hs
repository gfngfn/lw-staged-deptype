module Surface.BindingTime.Core
  ( BindingTimeVar (..),
    BindingTimeConst (..),
    BindingTime (..),
    BITypeF (..),
    BITypeMainF (..),
    BIType,
    BindingTimeEnvEntry (..),
    BindingTimeEnv,
    BExpr,
    BTypeExpr,
    BArgForType,
    BITypeVoid,
    fromStaged0,
    fromStaged1,
    fromStagedPers,
  )
where

import Data.Map (Map)
import GHC.Generics
import Lwsd.Syntax qualified as Lwsd
import Surface.Syntax
import Util.TokenUtil
import Prelude

newtype BindingTimeVar = BindingTimeVar Int
  deriving stock (Eq, Ord, Show)

data BindingTimeConst = BT0 | BT1
  deriving stock (Eq, Ord, Show) -- BT0 < BT1

data BindingTime
  = BTConst BindingTimeConst
  | BTVar BindingTimeVar
  deriving stock (Eq, Show, Generic)

-- Intermediate, minimal type representations for binding-time analysis
data BITypeF bt = BIType bt (BITypeMainF bt)
  deriving stock (Show)

data BITypeMainF bt
  = BITyBase [BITypeF bt]
  | BITyArrow (BITypeF bt) (BITypeF bt)
  | BITyOptArrow (BITypeF bt) (BITypeF bt)
  deriving stock (Show)

type BIType = BITypeF BindingTime

data BindingTimeEnvEntry
  = EntryBuiltInPersistent Var (BITypeF ())
  | EntryBuiltInFixed Var BindingTimeConst (BITypeF BindingTimeConst)
  | EntryLocallyBound BindingTime BIType
  | EntryModule BindingTimeEnv
  deriving stock (Show)

type BindingTimeEnv = Map Var BindingTimeEnvEntry

type BExpr = ExprF (BindingTime, Span)

type BTypeExpr = TypeExprF (BindingTime, Span)

type BArgForType = ArgForTypeF (BindingTime, Span)

-- For built-in values.
type BITypeVoid = BITypeF BindingTimeConst

fromStaged0 :: Lwsd.Ass0TypeExpr -> BITypeVoid
fromStaged0 = \case
  Lwsd.A0TyPrim _a0tyPrim _maybePred ->
    wrap0 $ BITyBase []
  Lwsd.A0TyList a0tye' _maybePred ->
    wrap0 $ BITyBase [fromStaged0 a0tye']
  Lwsd.A0TyArrow (_, a0tye1) a0tye2 ->
    wrap0 $ BITyArrow (fromStaged0 a0tye1) (fromStaged0 a0tye2)
  Lwsd.A0TyOptArrow (_, a0tye1) a0tye2 ->
    wrap0 $ BITyOptArrow (fromStaged0 a0tye1) (fromStaged0 a0tye2)
  Lwsd.A0TyCode a1tye ->
    fromStaged1 a1tye
  where
    wrap0 = BIType BT0

fromStaged1 :: Lwsd.Ass1TypeExpr -> BITypeVoid
fromStaged1 a1tye =
  BIType BT1 $
    case a1tye of
      Lwsd.A1TyPrim _a1tyPrim ->
        BITyBase []
      Lwsd.A1TyList a1tye' ->
        BITyBase [fromStaged1 a1tye']
      Lwsd.A1TyArrow a1tye1 a1tye2 ->
        BITyArrow (fromStaged1 a1tye1) (fromStaged1 a1tye2)

fromStagedPers :: Lwsd.AssPersTypeExpr -> BITypeF ()
fromStagedPers aPtye =
  BIType () $
    case aPtye of
      Lwsd.APersTyPrim _a0tyPrim ->
        BITyBase []
      Lwsd.APersTyList aPtye' ->
        BITyBase [fromStagedPers aPtye']
      Lwsd.APersTyArrow aPtye1 aPtye2 ->
        BITyArrow (fromStagedPers aPtye1) (fromStagedPers aPtye2)
