module Lwsd.BuiltIn
  ( tyNat,
    validateExternalName0,
    validateExternalName1,
  )
where

import Data.Text (Text)
import Lwsd.BuiltIn.Core
import Lwsd.Syntax
import Prelude

ass0exprIsNonnegative :: Ass0Expr
ass0exprIsNonnegative =
  A0App (A0BuiltInName (BuiltInArity2 BILeq)) (A0Literal (ALitInt 0))

tyNat :: Ass0TypeExpr
tyNat = A0TyPrim A0TyInt (Just ass0exprIsNonnegative)

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
