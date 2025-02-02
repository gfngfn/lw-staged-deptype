module Lwsd.BuiltIn.Core
  ( BuiltIn (..),
    BuiltInArity1 (..),
    BuiltInArity2 (..),
    BuiltInArity3 (..),
    Ass0PartialBuiltInApp (..),
    Ass1BuiltInName (..),
    validateExternalName0,
    validateExternalName1,
    unliftBuiltInName,
  )
where

import Data.Text (Text)
import Prelude

data BuiltIn
  = BuiltInArity1 BuiltInArity1
  | BuiltInArity2 BuiltInArity2
  | BuiltInArity3 BuiltInArity3
  deriving stock (Eq, Show)

data BuiltInArity1
  = BIGenVadd
  | BIMtranspose Int Int
  | BITensorGenZeros
  | BITensorGenGrad
  | BITensorGenZeroGrad
  | BITensorGenSubUpdate
  | BITensorGenCountEqual
  deriving stock (Eq, Show)

data BuiltInArity2
  = BIAdd
  | BISub
  | BIMult
  | BILeq
  | BIAnd
  | BIListMap
  | BIGenVconcat
  | BIGenMtranspose
  | BIVadd Int
  | BIVconcat Int Int
  | BIMconcatVert Int Int Int
  | BIDropAt
  | BIBroadcastable
  | BIBroadcast
  | BIListAppend
  | BIListIter
  | BIGenBroadcasted
  | BITensorGenAdd
  | BITensorGenMult
  | BITensorGenCrossEntropyForLogits
  | BITensorGenArgmax
  | BITensorAdd [Int]
  | BITensorMm Int Int Int
  deriving stock (Eq, Show)

data BuiltInArity3
  = BIGenMconcatVert
  | BITensorGenMm
  deriving stock (Eq, Show)

data Ass0PartialBuiltInApp v
  = A0PartialBuiltInApp1With0 BuiltInArity1
  | A0PartialBuiltInApp2With0 BuiltInArity2
  | A0PartialBuiltInApp2With1 BuiltInArity2 v
  | A0PartialBuiltInApp3With0 BuiltInArity3
  | A0PartialBuiltInApp3With1 BuiltInArity3 v
  | A0PartialBuiltInApp3With2 BuiltInArity3 v v
  deriving stock (Eq, Show)

data Ass1BuiltInName
  = A1BINameAdd
  | A1BINameSub
  | A1BINameMult
  | A1BINameFloatDiv
  | A1BINameLeq
  | A1BINameFloat
  | A1BINamePrintFloat
  | A1BINameListAppend
  | A1BINameListIter
  | A1BINameRange
  | A1BINameTensorF
  | A1BINameTensorBackward
  | A1BINameTensorNoGrad
  | A1BINameTensorFloatValue
  | A1BINameMnistHelperTrainImages
  | A1BINameMnistHelperTrainLabels
  | A1BINameMnistHelperTestImages
  | A1BINameMnistHelperTestLabels
  deriving stock (Eq, Show)

unliftBuiltInName :: Ass1BuiltInName -> BuiltIn
unliftBuiltInName = \case
  A1BINameAdd -> arity2 BIAdd
  A1BINameSub -> arity2 BISub
  A1BINameMult -> arity2 BIMult
  A1BINameLeq -> arity2 BILeq
  A1BINameListAppend -> arity2 BIListAppend
  A1BINameListIter -> arity2 BIListIter
  a1builtInName -> error $ "TODO: unliftBuiltInName, " ++ show a1builtInName
  where
    arity2 = BuiltInArity2

validateExternalName0 :: Text -> Maybe BuiltIn
validateExternalName0 = \case
  "int_add" -> arity2 BIAdd
  "int_sub" -> arity2 BISub
  "int_mult" -> arity2 BIMult
  "int_leq" -> arity2 BILeq
  "gen_vadd" -> arity1 BIGenVadd
  "gen_vconcat" -> arity2 BIGenVconcat
  "gen_mtranspose" -> arity2 BIGenMtranspose
  "gen_mconcat_vert" -> arity3 BIGenMconcatVert
  "drop_at" -> arity2 BIDropAt
  "broadcastable" -> arity2 BIBroadcastable
  "broadcast" -> arity2 BIBroadcast
  "list__append" -> arity2 BIListAppend
  "list__iter" -> arity2 BIListIter
  "gen_broadcasted" -> arity2 BIGenBroadcasted
  "tensor__gen_zeros" -> arity1 BITensorGenZeros
  "tensor__gen_add" -> arity2 BITensorGenAdd
  "tensor__gen_mult" -> arity2 BITensorGenMult
  "tensor__gen_mm" -> arity3 BITensorGenMm
  "tensor__gen_grad" -> arity1 BITensorGenGrad
  "tensor__gen_zero_grad" -> arity1 BITensorGenZeroGrad
  "tensor__gen_sub_update" -> arity1 BITensorGenSubUpdate
  "tensor__gen_argmax" -> arity2 BITensorGenArgmax
  "tensor__gen_cross_entropy_for_logits" -> arity2 BITensorGenCrossEntropyForLogits
  "tensor__gen_count_equal" -> arity1 BITensorGenCountEqual
  _ -> Nothing
  where
    arity1 = pure . BuiltInArity1
    arity2 = pure . BuiltInArity2
    arity3 = pure . BuiltInArity3

validateExternalName1 :: Text -> Maybe Ass1BuiltInName
validateExternalName1 = \case
  "int_add" -> pure A1BINameAdd
  "int_sub" -> pure A1BINameSub
  "int_mult" -> pure A1BINameMult
  "float_div" -> pure A1BINameFloatDiv
  "int_leq" -> pure A1BINameLeq
  "float" -> pure A1BINameFloat
  "print_float" -> pure A1BINamePrintFloat
  "list__append" -> pure A1BINameListAppend
  "list__iter" -> pure A1BINameListIter
  "range" -> pure A1BINameRange
  "tensor__f" -> pure A1BINameTensorF
  "tensor__backward" -> pure A1BINameTensorBackward
  "tensor__no_grad" -> pure A1BINameTensorNoGrad
  "tensor__float_value" -> pure A1BINameTensorFloatValue
  "mnist_helper__train_images" -> pure A1BINameMnistHelperTrainImages
  "mnist_helper__train_labels" -> pure A1BINameMnistHelperTrainLabels
  "mnist_helper__test_images" -> pure A1BINameMnistHelperTestImages
  "mnist_helper__test_labels" -> pure A1BINameMnistHelperTestLabels
  _ -> Nothing
