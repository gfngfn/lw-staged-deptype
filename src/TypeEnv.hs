module TypeEnv
  ( TypeEnv,
    empty,
    addVar,
    findVar,
  )
where

import Data.List.Extra qualified as List
import Syntax

newtype TypeEnv = TypeEnv [TypeEnvEntry]

data TypeEnvEntry = TypeEnvEntry Var TypeExpr

empty :: TypeEnv
empty = TypeEnv []

addVar :: Var -> TypeExpr -> TypeEnv -> TypeEnv
addVar var tye (TypeEnv revEntries) =
  TypeEnv (TypeEnvEntry var tye : revEntries)

findVar :: Var -> TypeEnv -> Maybe TypeExpr
findVar var0 (TypeEnv revEntries) =
  List.firstJust
    (\(TypeEnvEntry var tye) -> if var == var0 then Just tye else Nothing)
    revEntries
