module Lwsd.BuiltIn
  ( ass0exprAdd,
    ass0exprSub,
    ass0exprMult,
    ass0exprLeq,
    ass0exprAssertNat,
    ass0exprListMap,
    ass0exprVadd,
    ass0exprVconcat,
    ass0exprMtranspose,
    ass0exprMmult,
    ass0exprMconcatVert,
    ass0exprTadd,
    ass0valAdd,
    ass0valSub,
    ass0valMult,
    ass0valLeq,
    ass0valGenVadd,
    ass0valGenVconcat,
    ass0valGenMtranspose,
    ass0valGenMmult,
    ass0valGenMconcatVert,
    ass0valGenTadd,
    validateExternalName0,
    validateExternalName1,
  )
where

import Data.Map qualified as Map
import Data.Text (Text)
import Lwsd.Syntax
import Util.TokenUtil (Span)
import Prelude

tyValInt :: Ass0TypeVal
tyValInt = A0TyValPrim A0TyValInt

tyValNat :: Ass0TypeVal
tyValNat = A0TyValPrim A0TyValNat

styInt :: StrictAss0TypeExpr
styInt = SA0TyPrim A0TyInt

styNat :: StrictAss0TypeExpr
styNat = SA0TyPrim A0TyNat

sty0Vec :: Int -> StrictAss0TypeExpr
sty0Vec = SA0TyPrim . a0TyVec

sty0Mat :: Int -> Int -> StrictAss0TypeExpr
sty0Mat m n = SA0TyPrim (a0TyMat m n)

sty0Tensor :: [Int] -> StrictAss0TypeExpr
sty0Tensor = SA0TyPrim . A0TyTensor

-- Makes a closure equipped with the empty runtime environment.
clo :: AssVar -> Ass0TypeVal -> Ass0Expr -> Ass0Val
clo x a0tyv1 a0tye2 =
  A0ValLam Nothing (x, a0tyv1) a0tye2 Map.empty

lam :: AssVar -> StrictAss0TypeExpr -> Ass0Expr -> Ass0Expr
lam x sa0tye1 =
  A0Lam Nothing (x, sa0tye1)

ass0exprBinaryInt :: (AssVar -> AssVar -> BuiltIn) -> Ass0Expr
ass0exprBinaryInt f =
  lam x1 styInt $ lam x2 styInt $ A0AppBuiltIn (f x1 x2)
  where
    x1 = AssVar "x1"
    x2 = AssVar "x2"

ass0exprAdd, ass0exprSub, ass0exprMult, ass0exprLeq :: Ass0Expr
ass0exprAdd = ass0exprBinaryInt BIAdd
ass0exprSub = ass0exprBinaryInt BISub
ass0exprMult = ass0exprBinaryInt BIMult
ass0exprLeq = ass0exprBinaryInt BILeq

ass0exprAssertNat :: Span -> Ass0Expr
ass0exprAssertNat loc =
  lam x1 styInt $
    A0AppBuiltIn (BIAssertNat loc x1)
  where
    x1 = AssVar "n1"

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

ass0valBinaryInt :: (AssVar -> AssVar -> BuiltIn) -> Ass0Val
ass0valBinaryInt f =
  clo x1 tyValInt $
    lam x2 styInt $
      A0AppBuiltIn (f x1 x2)
  where
    x1 = AssVar "x1"
    x2 = AssVar "x2"

ass0exprTadd :: [Int] -> Ass0Expr
ass0exprTadd ns =
  lam x1 (sty0Tensor ns) $
    lam x2 (sty0Tensor ns) $
      A0AppBuiltIn (BITadd ns x1 x2)
  where
    x1 = AssVar "v1"
    x2 = AssVar "v2"

ass0valAdd, ass0valSub, ass0valMult, ass0valLeq :: Ass0Val
ass0valAdd = ass0valBinaryInt BIAdd
ass0valSub = ass0valBinaryInt BISub
ass0valMult = ass0valBinaryInt BIMult
ass0valLeq = ass0valBinaryInt BILeq

ass0valGenVadd :: Ass0Val
ass0valGenVadd =
  clo x1 tyValNat $
    A0AppBuiltIn (BIGenVadd x1)
  where
    x1 = AssVar "x1"

ass0valGenVconcat :: Ass0Val
ass0valGenVconcat =
  clo x1 tyValNat $
    lam x2 styNat $
      A0AppBuiltIn (BIGenVconcat x1 x2)
  where
    x1 = AssVar "x1"
    x2 = AssVar "x2"

ass0valGenMtranspose :: Ass0Val
ass0valGenMtranspose =
  clo x1 tyValNat $
    lam x2 styNat $
      A0AppBuiltIn (BIGenMtranspose x1 x2)
  where
    x1 = AssVar "x1"
    x2 = AssVar "x2"

ass0valGenMmult :: Ass0Val
ass0valGenMmult =
  clo x1 tyValNat $
    lam x2 styNat $
      lam x3 styNat $
        A0AppBuiltIn (BIGenMmult x1 x2 x3)
  where
    x1 = AssVar "x1"
    x2 = AssVar "x2"
    x3 = AssVar "x3"

ass0valGenMconcatVert :: Ass0Val
ass0valGenMconcatVert =
  clo x1 tyValNat $
    lam x2 styNat $
      lam x3 styNat $
        A0AppBuiltIn (BIGenMconcatVert x1 x2 x3)
  where
    x1 = AssVar "x1"
    x2 = AssVar "x2"
    x3 = AssVar "x3"

ass0valGenTadd :: Ass0Val
ass0valGenTadd =
  clo x1 tyValNat $
    A0AppBuiltIn (BIGenTadd x1)
  where
    x1 = AssVar "x1"

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
  "gen_tadd" -> pure A0BINameGenTadd
  "tensor__gen_f" -> pure A0BINameTensorGenF
  "tensor__gen_zeros" -> pure A0BINameTensorGenZeros
  "mnist_helper__image_dim" -> pure A0BINameMnistHelperImageDim
  "mnist_helper__label_count" -> pure A0BINameMnistHelperLabelCount
  _ -> Nothing

validateExternalName1 :: Text -> Maybe Ass1BuiltInName
validateExternalName1 = \case
  "int_add" -> pure A1BINameAdd
  "int_sub" -> pure A1BINameSub
  "int_mult" -> pure A1BINameMult
  "int_leq" -> pure A1BINameLeq
  "tensor__float_value" -> pure A1BINameTensorFloatValue
  _ -> Nothing
