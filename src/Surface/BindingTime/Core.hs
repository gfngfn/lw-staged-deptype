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
data BITypeF bt ann = BIType bt (BITypeMainF bt ann)
  deriving stock (Show)

data BITypeMainF bt ann
  = BITyBase
  | BITyArrow (BITypeF bt ann) (BITypeF bt ann)
  | BITyOptArrow (BITypeF bt ann) (BITypeF bt ann)
  deriving stock (Show)

type BIType = BITypeF BindingTime (BindingTime, Span)

data BindingTimeEnvEntry
  = EntryBuiltInPersistent (BITypeF () ())
  | EntryBuiltInFixed Var BindingTimeConst (BITypeF BindingTimeConst BindingTimeConst)
  | EntryLocallyBound BindingTime BIType
  deriving stock (Show)

type BindingTimeEnv = Map Var BindingTimeEnvEntry

type BExpr = ExprF (BindingTime, Span)

type BTypeExpr = TypeExprF (BindingTime, Span)

type BArgForType = ArgForTypeF (BindingTime, Span)
