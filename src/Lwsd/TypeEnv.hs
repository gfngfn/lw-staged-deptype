module Lwsd.TypeEnv
  ( TypeEnv,
    Ass0Metadata (..),
    Ass1Metadata (..),
    AssPersMetadata (..),
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
import Surface.Syntax qualified as SurfaceSyntax
import Prelude

newtype TypeEnv = TypeEnv [(Var, Entry)]

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

data Entry
  = Ass0Entry Ass0TypeExpr (Maybe Ass0Metadata)
  | Ass1Entry Ass1TypeExpr (Maybe Ass1Metadata)
  | AssPersEntry AssPersTypeExpr AssPersMetadata

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
