module Lwsd.BuiltIn
  ( tyNat,
    ass0exprAdd,
    ass0exprSub,
    ass0exprMult,
    ass0exprLeq,
    ass0exprListMap,
    ass0exprVadd,
    ass0exprVconcat,
    ass0exprMtranspose,
    ass0exprMmult,
    ass0exprMconcatVert,
    ass0exprTensorAdd,
    ass0exprAnd,
    getAss0Val,
    validateExternalName0,
    validateExternalName1,
  )
where

import Data.Map qualified as Map
import Data.Text (Text)
import Lwsd.Syntax
import Prelude

tyValInt :: Ass0TypeVal
tyValInt = A0TyValPrim A0TyValInt Nothing

tyValList :: Ass0TypeVal -> Ass0TypeVal
tyValList a0tyv = A0TyValList a0tyv Nothing

styInt :: StrictAss0TypeExpr
styInt = SA0TyPrim A0TyInt Nothing

styBool :: StrictAss0TypeExpr
styBool = SA0TyPrim A0TyBool Nothing

ass0exprIsNonnegative :: Ass0Expr
ass0exprIsNonnegative =
  lam n styInt $
    A0LetIn (zero, styInt) (A0Literal (ALitInt 0)) $
      A0AppBuiltIn (BILeq zero n)
  where
    n = AssVar "n"
    zero = AssVar "zero"

ass0valIsNonnegative :: Ass0Val
ass0valIsNonnegative =
  clo n tyValInt $
    A0LetIn (zero, styInt) (A0Literal (ALitInt 0)) $
      A0AppBuiltIn (BILeq zero n)
  where
    n = AssVar "n"
    zero = AssVar "zero"

tyNat :: Ass0TypeExpr
tyNat = A0TyPrim A0TyInt (Just ass0exprIsNonnegative)

tyValNat :: Ass0TypeVal
tyValNat = A0TyValPrim A0TyValInt (Just ass0valIsNonnegative)

styNat :: StrictAss0TypeExpr
styNat = SA0TyPrim A0TyInt (Just ass0exprIsNonnegative)

styUnit :: StrictAss0TypeExpr
styUnit = SA0TyPrim A0TyUnit Nothing

sty0Vec :: Int -> StrictAss0TypeExpr
sty0Vec n = SA0TyPrim (a0TyVec n) Nothing

sty0Mat :: Int -> Int -> StrictAss0TypeExpr
sty0Mat m n = SA0TyPrim (a0TyMat m n) Nothing

sty0Tensor :: [Int] -> StrictAss0TypeExpr
sty0Tensor ns = SA0TyPrim (A0TyTensor ns) Nothing

styList :: StrictAss0TypeExpr -> StrictAss0TypeExpr
styList sa0tye = SA0TyList sa0tye Nothing

-- Makes a closure equipped with the empty runtime environment.
clo :: AssVar -> Ass0TypeVal -> Ass0Expr -> Ass0Val
clo x a0tyv1 a0tye2 =
  A0ValLam Nothing (x, a0tyv1) a0tye2 Map.empty

lam :: AssVar -> StrictAss0TypeExpr -> Ass0Expr -> Ass0Expr
lam x sa0tye1 =
  A0Lam Nothing (x, sa0tye1)

ass0valBinaryInt :: (AssVar -> AssVar -> BuiltIn) -> Ass0Val
ass0valBinaryInt f =
  clo x1 tyValInt $
    lam x2 styInt $
      A0AppBuiltIn (f x1 x2)
  where
    x1 = AssVar "x1"
    x2 = AssVar "x2"

getAss0Val :: Ass0BuiltInName -> Ass0Val
getAss0Val = \case
  A0BINameAdd -> ass0valBinaryInt BIAdd
  A0BINameSub -> ass0valBinaryInt BISub
  A0BINameMult -> ass0valBinaryInt BIMult
  A0BINameLeq -> ass0valBinaryInt BILeq
  A0BINameGenVadd ->
    clo x1 tyValNat $
      A0AppBuiltIn (BIGenVadd x1)
    where
      x1 = AssVar "x1"
  A0BINameGenVconcat ->
    clo x1 tyValNat $
      lam x2 styNat $
        A0AppBuiltIn (BIGenVconcat x1 x2)
    where
      x1 = AssVar "x1"
      x2 = AssVar "x2"
  A0BINameGenMtranspose ->
    clo x1 tyValNat $
      lam x2 styNat $
        A0AppBuiltIn (BIGenMtranspose x1 x2)
    where
      x1 = AssVar "x1"
      x2 = AssVar "x2"
  A0BINameGenMmult ->
    clo x1 tyValNat $
      lam x2 styNat $
        lam x3 styNat $
          A0AppBuiltIn (BIGenMmult x1 x2 x3)
    where
      x1 = AssVar "x1"
      x2 = AssVar "x2"
      x3 = AssVar "x3"
  A0BINameGenMconcatVert ->
    clo x1 tyValNat $
      lam x2 styNat $
        lam x3 styNat $
          A0AppBuiltIn (BIGenMconcatVert x1 x2 x3)
    where
      x1 = AssVar "x1"
      x2 = AssVar "x2"
      x3 = AssVar "x3"
  A0BINameDropAt ->
    clo x1 tyValNat $
      lam x2 (styList styNat) $
        A0AppBuiltIn (BIDropAt x1 x2)
    where
      x1 = AssVar "x1"
      x2 = AssVar "x2"
  A0BINameListAppend ->
    -- TODO: generalize the type of `List.append`
    clo x1 (tyValList tyValNat) $
      lam x2 (styList styNat) $
        A0AppBuiltIn (BIListAppend x1 x2)
    where
      x1 = AssVar "x1"
      x2 = AssVar "x2"
  A0BINameListIter ->
    -- TODO: generalize the type of `List.append`
    clo x1 (A0TyValArrow (Nothing, tyValNat) styUnit) $
      lam x2 (styList styNat) $
        A0AppBuiltIn (BIListIter x1 x2)
    where
      x1 = AssVar "x1"
      x2 = AssVar "x2"
  A0BINameGenBroadcasted ->
    clo x1 (tyValList tyValNat) $
      lam x2 (styList styNat) $
        A0AppBuiltIn (BIGenBroadcasted x1 x2)
    where
      x1 = AssVar "x1"
      x2 = AssVar "x2"
  A0BINameTensorGenZeros ->
    clo x1 (tyValList tyValNat) $
      A0AppBuiltIn (BITensorGenZeros x1)
    where
      x1 = AssVar "x1"
  A0BINameTensorGenAdd ->
    clo x1 tyValNat $
      lam x2 styNat $
        A0AppBuiltIn (BITensorGenAdd x1 x2)
    where
      x1 = AssVar "x1"
      x2 = AssVar "x2"
  A0BINameTensorGenMult ->
    clo x1 (tyValList tyValNat) $
      A0AppBuiltIn (BITensorGenMult x1)
    where
      x1 = AssVar "x1"
  A0BINameTensorGenGrad ->
    clo x1 (tyValList tyValNat) $
      A0AppBuiltIn (BITensorGenGrad x1)
    where
      x1 = AssVar "x1"
  A0BINameTensorGenZeroGrad ->
    clo x1 (tyValList tyValNat) $
      A0AppBuiltIn (BITensorGenZeroGrad x1)
    where
      x1 = AssVar "x1"
  A0BINameTensorGenSubUpdate ->
    clo x1 (tyValList tyValNat) $
      A0AppBuiltIn (BITensorGenSubUpdate x1)
    where
      x1 = AssVar "x1"
  A0BINameTensorGenArgmax ->
    clo x1 (tyValList tyValNat) $
      lam x2 styNat $
        A0AppBuiltIn (BITensorGenArgmax x1 x2)
    where
      x1 = AssVar "x1"
      x2 = AssVar "x2"
  A0BINameTensorGenCrossEntropyForLogits ->
    clo x1 tyValNat $
      lam x2 styNat $
        A0AppBuiltIn (BITensorGenCrossEntropyForLogits x1 x2)
    where
      x1 = AssVar "x1"
      x2 = AssVar "x2"
  A0BINameTensorGenCountEqual ->
    clo x1 (tyValList tyValNat) $
      A0AppBuiltIn (BITensorGenCountEqual x1)
    where
      x1 = AssVar "x1"
  a0builtInName ->
    error $ "TODO: getAss0Val, " ++ show a0builtInName

ass0exprBinaryInt :: (AssVar -> AssVar -> BuiltIn) -> Ass0Expr
ass0exprBinaryInt f =
  lam x1 styInt $
    lam x2 styInt $
      A0AppBuiltIn (f x1 x2)
  where
    x1 = AssVar "x1"
    x2 = AssVar "x2"

ass0exprAdd, ass0exprSub, ass0exprMult, ass0exprLeq :: Ass0Expr
ass0exprAdd = ass0exprBinaryInt BIAdd
ass0exprSub = ass0exprBinaryInt BISub
ass0exprMult = ass0exprBinaryInt BIMult
ass0exprLeq = ass0exprBinaryInt BILeq

ass0exprListMap :: StrictAss0TypeExpr -> StrictAss0TypeExpr -> Ass0Expr
ass0exprListMap styDom styCod =
  lam f (SA0TyArrow (Nothing, styDom) styCod) $
    lam x styDom $
      A0AppBuiltIn (BIListMap f x)
  where
    f = AssVar "f"
    x = AssVar "x"

ass0exprVadd :: Int -> Ass0Expr
ass0exprVadd n =
  lam x1 (sty0Vec n) $
    lam x2 (sty0Vec n) $
      A0AppBuiltIn (BIVadd n x1 x2)
  where
    x1 = AssVar "v1"
    x2 = AssVar "v2"

ass0exprVconcat :: Int -> Int -> Ass0Expr
ass0exprVconcat m n =
  lam x1 (sty0Vec m) $
    lam x2 (sty0Vec n) $
      A0AppBuiltIn (BIVconcat m n x1 x2)
  where
    x1 = AssVar "v1"
    x2 = AssVar "v2"

ass0exprMtranspose :: Int -> Int -> Ass0Expr
ass0exprMtranspose m n =
  lam x1 (sty0Mat m n) (A0AppBuiltIn (BIMtranspose m n x1))
  where
    x1 = AssVar "mat1"

ass0exprMconcatVert :: Int -> Int -> Int -> Ass0Expr
ass0exprMconcatVert m1 m2 n =
  lam x1 (sty0Mat m1 n) $
    lam x2 (sty0Mat m2 n) $
      A0AppBuiltIn (BIMconcatVert m1 m2 n x1 x2)
  where
    x1 = AssVar "mat1"
    x2 = AssVar "mat2"

ass0exprMmult :: Int -> Int -> Int -> Ass0Expr
ass0exprMmult k m n =
  lam x1 (sty0Mat k m) $
    lam x2 (sty0Mat m n) $
      A0AppBuiltIn (BIMmult k m n x1 x2)
  where
    x1 = AssVar "mat1"
    x2 = AssVar "mat2"

ass0exprTensorAdd :: [Int] -> Ass0Expr
ass0exprTensorAdd ns =
  lam x1 (sty0Tensor ns) $
    lam x2 (sty0Tensor ns) $
      A0AppBuiltIn (BITensorAdd ns x1 x2)
  where
    x1 = AssVar "v1"
    x2 = AssVar "v2"

ass0exprAnd :: Ass0Expr
ass0exprAnd =
  lam x1 styBool $
    lam x2 styBool $
      A0AppBuiltIn (BIAnd x1 x2)
  where
    x1 = AssVar "x1"
    x2 = AssVar "x2"

validateExternalName0 :: Text -> Maybe Ass0BuiltInName
validateExternalName0 = \case
  "int_add" -> pure A0BINameAdd
  "int_sub" -> pure A0BINameSub
  "int_mult" -> pure A0BINameMult
  "int_leq" -> pure A0BINameLeq
  "gen_vadd" -> pure A0BINameGenVadd
  "gen_vconcat" -> pure A0BINameGenVconcat
  "gen_mtranspose" -> pure A0BINameGenMtranspose
  "gen_mmult" -> pure A0BINameGenMmult
  "gen_mconcat_vert" -> pure A0BINameGenMconcatVert
  "drop_at" -> pure A0BINameDropAt
  "broadcastable" -> pure A0BINameBroadcastable
  "broadcast" -> pure A0BINameBroadcast
  "gen_broadcasted" -> pure A0BINameGenBroadcasted
  "tensor__gen_zeros" -> pure A0BINameTensorGenZeros
  "tensor__gen_add" -> pure A0BINameTensorGenAdd
  "tensor__gen_mult" -> pure A0BINameTensorGenMult
  "tensor__gen_grad" -> pure A0BINameTensorGenGrad
  "tensor__gen_zero_grad" -> pure A0BINameTensorGenZeroGrad
  "tensor__gen_sub_update" -> pure A0BINameTensorGenSubUpdate
  "tensor__gen_argmax" -> pure A0BINameTensorGenArgmax
  "tensor__gen_cross_entropy_for_logits" -> pure A0BINameTensorGenCrossEntropyForLogits
  "tensor__gen_count_equal" -> pure A0BINameTensorGenCountEqual
  _ -> Nothing

validateExternalName1 :: Text -> Maybe Ass1BuiltInName
validateExternalName1 = \case
  "int_add" -> pure A1BINameAdd
  "int_sub" -> pure A1BINameSub
  "int_mult" -> pure A1BINameMult
  "float_div" -> pure A1BINameFloatDiv
  "int_leq" -> pure A1BINameLeq
  "float" -> pure A1BINameFloat
  "print_float" -> pure A1BINamePrintFloat
  "list_append" -> pure A1BINameListAppend
  "list_iter" -> pure A1BINameListIter
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
