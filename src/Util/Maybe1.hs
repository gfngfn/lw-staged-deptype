module Util.Maybe1
  ( Maybe1 (..),
  )
where

import Prelude

newtype Maybe1 af sv = Maybe1 {unMaybe1 :: Maybe (af sv)}
