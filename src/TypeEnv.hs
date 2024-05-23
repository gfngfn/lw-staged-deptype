module TypeEnv
  ( TypeEnv,
    Entry (..),
    empty,
    addVar,
    findVar,
  )
where

import Data.List.Extra qualified as List
import Syntax

newtype TypeEnv = TypeEnv [(Var, Entry)]

data Entry
  = Ass0Entry Ass0TypeExpr
  | Ass1Entry Ass1TypeExpr

empty :: TypeEnv
empty = TypeEnv []

addVar :: Var -> Entry -> TypeEnv -> TypeEnv
addVar var entry (TypeEnv revEntries) =
  TypeEnv ((var, entry) : revEntries)

findVar :: Var -> TypeEnv -> Maybe Entry
findVar var0 (TypeEnv revEntries) =
  List.firstJust
    (\(var, entry) -> if var == var0 then Just entry else Nothing)
    revEntries
