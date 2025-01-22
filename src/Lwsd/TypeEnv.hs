module Lwsd.TypeEnv
  ( TypeEnv,
    Ass0Metadata (..),
    Ass1Metadata (..),
    AssPersMetadata (..),
    ValEntry (..),
    ModuleEntry (..),
    SigRecord,
    empty,
    addVar,
    findVar,
    addModule,
    emptySigRecord,
    singletonVal,
    singletonModule,
    intersectSigRecord,
    mergeSigRecord,
    foldSigRecord,
    appendSigRecord,
  )
where

import Data.List.Extra qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Lwsd.SrcSyntax (Var)
import Lwsd.Syntax
import Surface.Syntax qualified as SurfaceSyntax
import Prelude

data TypeEnv = TypeEnv
  { envVals :: [(Var, ValEntry)],
    envModules :: [(Var, ModuleEntry)]
  }

data Ass0Metadata = Ass0Metadata
  { ass0builtInName :: Ass0BuiltInName,
    ass0surfaceName :: SurfaceSyntax.Var
  }

data Ass1Metadata = Ass1Metadata
  { ass1builtInName :: Ass1BuiltInName,
    ass1surfaceName :: SurfaceSyntax.Var
  }

data AssPersMetadata = AssPersMetadata
  { assPbuiltInName :: Ass1BuiltInName,
    assPsurfaceName :: SurfaceSyntax.Var
  }

data ValEntry
  = Ass0Entry Ass0TypeExpr (Maybe Ass0Metadata)
  | Ass1Entry Ass1TypeExpr (Maybe Ass1Metadata)
  | AssPersEntry AssPersTypeExpr AssPersMetadata

newtype ModuleEntry
  = ModuleEntry SigRecord

data SigRecord = SigRecord
  { sigVals :: Map Var ValEntry,
    sigModules :: Map Var ModuleEntry
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

emptySigRecord :: SigRecord
emptySigRecord = SigRecord Map.empty Map.empty

singletonVal :: Var -> ValEntry -> SigRecord
singletonVal var entry = SigRecord (Map.singleton var entry) Map.empty

singletonModule :: Var -> ModuleEntry -> SigRecord
singletonModule m modEntry = SigRecord Map.empty (Map.singleton m modEntry)

intersectSigRecord :: SigRecord -> SigRecord -> ([Var], [Var])
intersectSigRecord (SigRecord vals1 modules1) (SigRecord vals2 modules2) =
  (map fst $ Map.toList (Map.intersection vals1 vals2), map fst $ Map.toList (Map.intersection modules1 modules2))

mergeSigRecord :: SigRecord -> SigRecord -> SigRecord
mergeSigRecord (SigRecord vals1 modules1) (SigRecord vals2 modules2) =
  SigRecord (Map.union vals1 vals2) (Map.union modules1 modules2)

foldSigRecord :: (Var -> ValEntry -> a -> a) -> (Var -> ModuleEntry -> a -> a) -> a -> SigRecord -> a
foldSigRecord fVal fModule acc (SigRecord {sigVals, sigModules}) =
  Map.foldrWithKey fModule (Map.foldrWithKey fVal acc sigVals) sigModules

appendSigRecord :: TypeEnv -> SigRecord -> TypeEnv
appendSigRecord tyEnv SigRecord {sigVals, sigModules} =
  Map.foldrWithKey addModule (Map.foldrWithKey addVar tyEnv sigVals) sigModules
