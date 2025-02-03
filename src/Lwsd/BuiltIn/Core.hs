module Lwsd.BuiltIn.Core
  ( BuiltIn (..),
    BuiltInArity1 (..),
    BuiltInArity2 (..),
    BuiltInArity3 (..),
    Ass0PartialBuiltInApp (..),
    Ass1BuiltIn (..),
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

data Ass0PartialBuiltInApp val
  = A0PartialBuiltInApp1With0 BuiltInArity1
  | A0PartialBuiltInApp2With0 BuiltInArity2
  | A0PartialBuiltInApp2With1 BuiltInArity2 val
  | A0PartialBuiltInApp3With0 BuiltInArity3
  | A0PartialBuiltInApp3With1 BuiltInArity3 val
  | A0PartialBuiltInApp3With2 BuiltInArity3 val val
  deriving stock (Eq, Show, Functor)

data Ass1BuiltIn
  = A1BIVadd Int
  | A1BIVconcat Int Int
  | A1BIMtranspose Int Int
  | A1BIMconcatVert Int Int Int
  | A1BITensorZeros [Int]
  | A1BITensorAdd [Int] [Int]
  | A1BITensorMult [Int] [Int]
  | A1BITensorMm Int Int Int
  | A1BITensorGrad [Int]
  | A1BITensorZeroGrad [Int]
  | A1BITensorSubUpdate [Int]
  | A1BITensorArgmax [Int] Int
  | A1BITensorCrossEntropyForLogits Int Int
  | A1BITensorCountEqual [Int]
  | A1BIAdd
  | A1BISub
  | A1BIMult
  | A1BIFloatDiv
  | A1BILeq
  | A1BIFloat
  | A1BIPrintFloat
  | A1BIRange
  | A1BIListAppend
  | A1BIListIter
  | A1BITensorF
  | A1BITensorBackward
  | A1BITensorNoGrad
  | A1BITensorFloatValue
  | A1BIMnistHelperTrainImages
  | A1BIMnistHelperTrainLabels
  | A1BIMnistHelperTestImages
  | A1BIMnistHelperTestLabels
  deriving stock (Eq, Show)

unliftBuiltInName :: Ass1BuiltIn -> BuiltIn
unliftBuiltInName = \case
  A1BIVadd n -> arity2 (BIVadd n)
  A1BIVconcat m n -> arity2 (BIVconcat m n)
  A1BIMtranspose m n -> arity1 (BIMtranspose m n)
  A1BIMconcatVert m1 m2 n -> arity2 (BIMconcatVert m1 m2 n)
  A1BITensorAdd ns1 ns2 ->
    if ns1 == ns2
      then arity2 (BITensorAdd ns1)
      else error $ "TODO: unliftVal, A1BITensorAdd, broadcast, " ++ show ns1 ++ " and " ++ show ns2
  A1BITensorMm k m n -> arity2 (BITensorMm k m n)
  A1BIAdd -> arity2 BIAdd
  A1BISub -> arity2 BISub
  A1BIMult -> arity2 BIMult
  A1BILeq -> arity2 BILeq
  A1BIListAppend -> arity2 BIListAppend
  A1BIListIter -> arity2 BIListIter
  a1builtInName -> error $ "TODO: unliftBuiltInName, " ++ show a1builtInName
  where
    arity1 = BuiltInArity1
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

validateExternalName1 :: Text -> Maybe Ass1BuiltIn
validateExternalName1 = \case
  "int_add" -> pure A1BIAdd
  "int_sub" -> pure A1BISub
  "int_mult" -> pure A1BIMult
  "float_div" -> pure A1BIFloatDiv
  "int_leq" -> pure A1BILeq
  "float" -> pure A1BIFloat
  "print_float" -> pure A1BIPrintFloat
  "range" -> pure A1BIRange
  "list__append" -> pure A1BIListAppend
  "list__iter" -> pure A1BIListIter
  "tensor__f" -> pure A1BITensorF
  "tensor__backward" -> pure A1BITensorBackward
  "tensor__no_grad" -> pure A1BITensorNoGrad
  "tensor__float_value" -> pure A1BITensorFloatValue
  "mnist_helper__train_images" -> pure A1BIMnistHelperTrainImages
  "mnist_helper__train_labels" -> pure A1BIMnistHelperTrainLabels
  "mnist_helper__test_images" -> pure A1BIMnistHelperTestImages
  "mnist_helper__test_labels" -> pure A1BIMnistHelperTestLabels
  _ -> Nothing
