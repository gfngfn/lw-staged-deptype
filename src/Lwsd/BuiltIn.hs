module Lwsd.BuiltIn
  ( ass0exprAssertNat,
    ass0exprVadd,
    ass0exprVconcat,
    ass0exprMtranspose,
    ass0exprMmult,
    ass0exprMconcatVert,
    initialTypeEnv,
    initialEnv,
  )
where

import Data.List qualified as List
import Data.Map qualified as Map
import Lwsd.Syntax
import Lwsd.TypeEnv (TypeEnv)
import Lwsd.TypeEnv qualified as TypeEnv
import Util.TokenUtil (Span)
import Prelude

tyInt :: Ass0TypeExpr
tyInt = A0TyPrim A0TyInt

tyNat :: Ass0TypeExpr
tyNat = A0TyPrim A0TyNat

tyBool :: Ass0TypeExpr
tyBool = A0TyPrim A0TyBool

ty1Vec :: Ass0Expr -> Ass1TypeExpr
ty1Vec = A1TyPrim . A1TyVec

ty1Mat :: Ass0Expr -> Ass0Expr -> Ass1TypeExpr
ty1Mat a0e1 a0e2 = A1TyPrim (A1TyMat a0e1 a0e2)

(-->) :: Ass0TypeExpr -> Ass0TypeExpr -> Ass0TypeExpr
(-->) a0tye1 = A0TyArrow (Nothing, a0tye1)

infixr 0 -->

(-:>) :: (AssVar, Ass0TypeExpr) -> Ass0TypeExpr -> Ass0TypeExpr
(-:>) (x, a0tye1) = A0TyOptArrow (x, a0tye1)

infixr 0 -:>

(==>) :: Ass1TypeExpr -> Ass1TypeExpr -> Ass1TypeExpr
(==>) = A1TyArrow

infixr 0 ==>

initialTypeEnv :: TypeEnv
initialTypeEnv =
  List.foldl'
    (\tyEnv (x, a0tye) -> TypeEnv.addVar x (TypeEnv.Ass0Entry a0tye) tyEnv)
    TypeEnv.empty
    [ ("+", tyInt --> tyInt --> tyInt),
      ("-", tyInt --> tyInt --> tyInt),
      ("*", tyInt --> tyInt --> tyInt),
      ("<=", tyInt --> tyInt --> tyBool),
      ("assert_nat", tyInt --> tyNat),
      ("gen_vadd", tyGenVadd),
      ("gen_vconcat", tyGenVconcat),
      ("gen_mtranspose", tyGenMtranspose),
      ("gen_mmult", tyGenMmult),
      ("gen_mconcat_vert", tyGenMconcatVert)
    ]
  where
    tyGenVadd :: Ass0TypeExpr
    tyGenVadd =
      (a, tyInt)
        -:> A0TyCode (ty1Vec (A0Var a) ==> ty1Vec (A0Var a) ==> ty1Vec (A0Var a))
      where
        a = AssVar "a"

    tyGenVconcat :: Ass0TypeExpr
    tyGenVconcat =
      (a, tyInt)
        -:> (b, tyInt)
        -:> A0TyCode
          ( ty1Vec (A0Var a)
              ==> ty1Vec (A0Var b)
              ==> ty1Vec (a0add (A0Var a) (A0Var b))
          )
      where
        a = AssVar "a"
        b = AssVar "b"

    tyGenMtranspose :: Ass0TypeExpr
    tyGenMtranspose =
      (a, tyInt)
        -:> (b, tyInt)
        -:> A0TyCode
          ( ty1Mat (A0Var a) (A0Var b)
              ==> ty1Mat (A0Var b) (A0Var a)
          )
      where
        a = AssVar "a"
        b = AssVar "b"

    tyGenMmult :: Ass0TypeExpr
    tyGenMmult =
      (a, tyInt)
        -:> (b, tyInt)
        -:> (c, tyInt)
        -:> A0TyCode
          ( ty1Mat (A0Var a) (A0Var b)
              ==> ty1Mat (A0Var b) (A0Var c)
              ==> ty1Mat (A0Var a) (A0Var c)
          )
      where
        a = AssVar "a"
        b = AssVar "b"
        c = AssVar "c"

    tyGenMconcatVert :: Ass0TypeExpr
    tyGenMconcatVert =
      (a, tyInt)
        -:> (b, tyInt)
        -:> (c, tyInt)
        -:> A0TyCode
          ( ty1Mat (A0Var a) (A0Var c)
              ==> ty1Mat (A0Var b) (A0Var c)
              ==> ty1Mat (a0add (A0Var a) (A0Var b)) (A0Var c)
          )
      where
        a = AssVar "a"
        b = AssVar "b"
        c = AssVar "c"

    a0add :: Ass0Expr -> Ass0Expr -> Ass0Expr
    a0add a0e1 = A0App (A0App (A0Var (AssVar "+")) a0e1)

tyValInt :: Ass0TypeVal
tyValInt = A0TyValPrim A0TyValInt

tyValNat :: Ass0TypeVal
tyValNat = A0TyValPrim A0TyValNat

styInt :: StrictAss0TypeExpr
styInt = SA0TyPrim A0TyInt

styNat :: StrictAss0TypeExpr
styNat = SA0TyPrim A0TyNat

sty0Vec :: Int -> StrictAss0TypeExpr
sty0Vec = SA0TyPrim . A0TyVec

sty0Mat :: Int -> Int -> StrictAss0TypeExpr
sty0Mat m n = SA0TyPrim (A0TyMat m n)

-- Makes a closure equipped with `initialEnv`.
clo :: AssVar -> Ass0TypeVal -> Ass0Expr -> Ass0Val
clo x a0tyv1 a0tye2 = A0ValLam Nothing (x, a0tyv1) a0tye2 initialEnv

lam :: AssVar -> StrictAss0TypeExpr -> Ass0Expr -> Ass0Expr
lam x sa0tye1 = A0Lam Nothing (x, sa0tye1)

ass0exprAssertNat :: Span -> Ass0Expr
ass0exprAssertNat loc =
  lam x1 styInt $
    A0AppBuiltIn (BIAssertNat loc x1)
  where
    x1 = AssVar "n1"

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

ass0valAdd :: Ass0Val
ass0valAdd = ass0valBinaryInt BIAdd

ass0valSub :: Ass0Val
ass0valSub = ass0valBinaryInt BISub

ass0valMult :: Ass0Val
ass0valMult = ass0valBinaryInt BIMult

ass0valLeq :: Ass0Val
ass0valLeq = ass0valBinaryInt BILeq

ass0valGenVadd :: Ass0Val
ass0valGenVadd =
  clo x1 tyValInt $
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

initialEnv :: Env0
initialEnv =
  List.foldl'
    (\env (x, a0v) -> Map.insert (AssVar x) (Ass0ValEntry a0v) env)
    Map.empty
    [ ("+", ass0valAdd),
      ("-", ass0valSub),
      ("*", ass0valMult),
      ("<=", ass0valLeq),
      ("gen_vadd", ass0valGenVadd),
      ("gen_vconcat", ass0valGenVconcat),
      ("gen_mtranspose", ass0valGenMtranspose),
      ("gen_mmult", ass0valGenMmult),
      ("gen_mconcat_vert", ass0valGenMconcatVert)
    ]
