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
  )
where

import Data.Map (Map)
import GHC.Generics
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
  = EntryBuiltInPersistent (BITypeF ())
  | EntryBuiltInFixed Var BindingTimeConst (BITypeF BindingTimeConst)
  | EntryLocallyBound BindingTime BIType
  deriving stock (Show)

type BindingTimeEnv = Map Var BindingTimeEnvEntry

type BExpr = ExprF (BindingTime, Span)

type BTypeExpr = TypeExprF (BindingTime, Span)

type BArgForType = ArgForTypeF (BindingTime, Span)
