module Lwsd.Scope.TypeEnv
  ( TypeEnv,
    empty,
    addVar,
    findVar,
    addModule,
    appendSigRecord,
  )
where

import Data.List.Extra qualified as List
import Lwsd.Scope.SigRecord (ModuleEntry, SigRecord, ValEntry)
import Lwsd.Scope.SigRecord qualified as SigRecord
import Lwsd.SrcSyntax (Var)
import Prelude

-- TODO (enhance): optimize internal representation
data TypeEnv = TypeEnv
  { envVals :: [(Var, ValEntry)],
    envModules :: [(Var, ModuleEntry)]
  }

empty :: TypeEnv
empty = TypeEnv {envVals = [], envModules = []}

addVar :: Var -> ValEntry -> TypeEnv -> TypeEnv
addVar x valEntry (TypeEnv revValEntries revModEntries) =
  TypeEnv ((x, valEntry) : revValEntries) revModEntries

findVar :: Var -> TypeEnv -> Maybe ValEntry
findVar var0 (TypeEnv revValEntries _revModEntries) =
  List.firstJust
    (\(var, entry) -> if var == var0 then Just entry else Nothing)
    revValEntries

addModule :: Var -> ModuleEntry -> TypeEnv -> TypeEnv
addModule m modEntry (TypeEnv revValEntries revModEntries) =
  TypeEnv revValEntries ((m, modEntry) : revModEntries)

appendSigRecord :: TypeEnv -> SigRecord -> TypeEnv
appendSigRecord =
  SigRecord.fold addVar addModule
