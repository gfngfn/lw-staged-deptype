module Lwsd.Scope.TypeEnv
  ( TypeEnv,
    empty,
    addVal,
    findVal,
    addModule,
    findModule,
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

addVal :: Var -> ValEntry -> TypeEnv -> TypeEnv
addVal x valEntry (TypeEnv revValEntries revModEntries) =
  TypeEnv ((x, valEntry) : revValEntries) revModEntries

findVal :: Var -> TypeEnv -> Maybe ValEntry
findVal x0 (TypeEnv revValEntries _revModEntries) =
  List.firstJust
    (\(x, valEntry) -> if x == x0 then Just valEntry else Nothing)
    revValEntries

addModule :: Var -> ModuleEntry -> TypeEnv -> TypeEnv
addModule m modEntry (TypeEnv revValEntries revModEntries) =
  TypeEnv revValEntries ((m, modEntry) : revModEntries)

findModule :: Var -> TypeEnv -> Maybe ModuleEntry
findModule m0 (TypeEnv _revValEntries revModEntries) =
  List.firstJust
    (\(m, modEntry) -> if m == m0 then Just modEntry else Nothing)
    revModEntries

appendSigRecord :: TypeEnv -> SigRecord -> TypeEnv
appendSigRecord =
  SigRecord.fold addVal addModule
