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

data TypeEnv = TypeEnv
  { envVals :: [(Var, ValEntry)],
    envModules :: [(Var, ModuleEntry)]
  }

empty :: TypeEnv
empty = TypeEnv {envVals = [], envModules = []}

addVar :: Var -> ValEntry -> TypeEnv -> TypeEnv
addVar var entry (TypeEnv revValEntries revModEntries) =
  TypeEnv ((var, entry) : revValEntries) revModEntries

findVar :: Var -> TypeEnv -> Maybe ValEntry
findVar var0 (TypeEnv revEntries _revModEntries) =
  List.firstJust
    (\(var, entry) -> if var == var0 then Just entry else Nothing)
    revEntries

addModule :: Var -> ModuleEntry -> TypeEnv -> TypeEnv
addModule = error "TODO: TypeEnv.addModule"

appendSigRecord :: TypeEnv -> SigRecord -> TypeEnv
appendSigRecord =
  SigRecord.fold addVar addModule
