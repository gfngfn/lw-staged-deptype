module Lwsd.TypeEnv
  ( TypeEnv,
    Entry (..),
    SigRecord,
    empty,
    addVar,
    findVar,
    appendSigRecord,
  )
where

import Data.List.Extra qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Lwsd.SrcSyntax (Var)
import Lwsd.Syntax
import Prelude

newtype TypeEnv = TypeEnv [(Var, Entry)]

data Entry
  = Ass0Entry Ass0TypeExpr
  | Ass1Entry Ass1TypeExpr

type SigRecord = Map Var Entry

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

appendSigRecord :: TypeEnv -> SigRecord -> TypeEnv
appendSigRecord = Map.foldrWithKey addVar
