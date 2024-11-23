module Surface.BuiltIn
  ( initialBindingTimeEnv,
  )
where

import Data.List qualified as List
import Data.Map qualified as Map
import Surface.BindingTime.Core (BITypeF (..), BITypeMainF (..), BindingTimeConst (..), BindingTimeEnv, BindingTimeEnvEntry (..))
import Prelude

type BITypeVoid = BITypeF BindingTimeConst BindingTimeConst

wrap0 :: BITypeMainF BindingTimeConst BindingTimeConst -> BITypeVoid
wrap0 = BIType BT0

wrap1 :: BITypeMainF BindingTimeConst BindingTimeConst -> BITypeVoid
wrap1 = BIType BT1

base0 :: BITypeVoid
base0 = wrap0 BITyBase

base1 :: BITypeVoid
base1 = wrap1 BITyBase

(-->) :: BITypeVoid -> BITypeVoid -> BITypeVoid
(-->) bity1 bity2 = wrap1 $ BITyArrow bity1 bity2

infixr 0 -->

(-?>) :: BITypeVoid -> BITypeVoid -> BITypeVoid
(-?>) bity1 bity2 = wrap0 $ BITyOptArrow bity1 bity2

infixr 0 -?>

initialBindingTimeEnv :: BindingTimeEnv
initialBindingTimeEnv =
  List.foldl'
    (\btenv (x, entry) -> Map.insert x entry btenv)
    Map.empty
    [ ("+", persistent bityBinary),
      ("-", persistent bityBinary),
      ("*", persistent bityBinary),
      ("<=", persistent bityBinary),
      ("vadd", fixed0 "gen_vadd" bityVadd),
      ("vconcat", fixed0 "gen_vconcat" bityVconcat),
      ("mtranspose", fixed0 "gen_mtranspose" bityMtranspose),
      ("mmult", fixed0 "gen_mmult" bityMmult),
      ("mconcat_vert", fixed0 "gen_mconcat_vert" bityMconcatVert)
    ]
  where
    persistent = EntryBuiltInPersistent
    fixed0 x' = EntryBuiltInFixed x' BT0

    bityBinary :: BITypeF () ()
    bityBinary =
      base `arrow` (base `arrow` base)
      where
        wrap = BIType ()
        base = wrap BITyBase
        arrow bity1 bity2 = wrap $ BITyArrow bity1 bity2

    bityVadd :: BITypeVoid
    bityVadd = base0 -?> base1 --> base1 --> base1

    bityVconcat :: BITypeVoid
    bityVconcat = base0 -?> base0 -?> base1 --> base1 --> base1

    bityMtranspose :: BITypeVoid
    bityMtranspose = base0 -?> base0 -?> base1 --> base1

    bityMmult :: BITypeVoid
    bityMmult = base0 -?> base0 -?> base0 -?> base1 --> base1 --> base1

    bityMconcatVert :: BITypeVoid
    bityMconcatVert = base0 -?> base0 -?> base0 -?> base1 --> base1 --> base1
