module Lwsd.BuiltIn
  ( ass0exprVadd,
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

tyInt :: Ass0TypeExpr
tyInt = A0TyPrim A0TyInt

ty1Vec :: Ass0Expr -> Ass1TypeExpr
ty1Vec = A1TyPrim . A1TyVec

ty0Vec :: Int -> Ass0TypeExpr
ty0Vec = A0TyPrim . A0TyVec

ty1Mat :: Ass0Expr -> Ass0Expr -> Ass1TypeExpr
ty1Mat a0e1 a0e2 = A1TyPrim (A1TyMat a0e1 a0e2)

ty0Mat :: Int -> Int -> Ass0TypeExpr
ty0Mat m n = A0TyPrim (A0TyMat m n)

(-->) :: Ass0TypeExpr -> Ass0TypeExpr -> Ass0TypeExpr
(-->) a0tye1 = A0TyArrow (Nothing, a0tye1)

infixr 0 -->

(-:>) :: (Var, Ass0TypeExpr) -> Ass0TypeExpr -> Ass0TypeExpr
(-:>) (x, a0tye1) = A0TyArrow (Just x, a0tye1)

infixr 0 -:>

(==>) :: Ass1TypeExpr -> Ass1TypeExpr -> Ass1TypeExpr
(==>) = A1TyArrow

infixr 0 ==>

initialTypeEnv :: TypeEnv
initialTypeEnv =
  List.foldl'
    (\tyEnv (x, a0tye) -> TypeEnv.addVar x (TypeEnv.Ass0Entry a0tye) tyEnv)
    TypeEnv.empty
    [ ("add", tyInt --> tyInt --> tyInt),
      ("sub", tyInt --> tyInt --> tyInt),
      ("mult", tyInt --> tyInt --> tyInt),
      ("gen_vadd", tyGenVadd),
      ("gen_vconcat", tyGenVconcat),
      ("gen_mtranspose", tyGenMtranspose),
      ("gen_mmult", tyGenMmult),
      ("gen_mconcat_vert", tyGenMconcatVert)
    ]
  where
    tyGenVadd :: Ass0TypeExpr
    tyGenVadd =
      ("a", tyInt)
        -:> A0TyCode (ty1Vec (A0Var "a") ==> ty1Vec (A0Var "a") ==> ty1Vec (A0Var "a"))

    tyGenVconcat :: Ass0TypeExpr
    tyGenVconcat =
      ("a", tyInt)
        -:> ("b", tyInt)
        -:> A0TyCode
          ( ty1Vec (A0Var "a")
              ==> ty1Vec (A0Var "b")
              ==> ty1Vec (a0add (A0Var "a") (A0Var "b"))
          )

    tyGenMtranspose :: Ass0TypeExpr
    tyGenMtranspose =
      ("a", tyInt)
        -:> ("b", tyInt)
        -:> A0TyCode
          ( ty1Mat (A0Var "a") (A0Var "b")
              ==> ty1Mat (A0Var "b") (A0Var "a")
          )

    tyGenMmult :: Ass0TypeExpr
    tyGenMmult =
      ("a", tyInt)
        -:> ("b", tyInt)
        -:> ("c", tyInt)
        -:> A0TyCode
          ( ty1Mat (A0Var "a") (A0Var "b")
              ==> ty1Mat (A0Var "b") (A0Var "c")
              ==> ty1Mat (A0Var "a") (A0Var "c")
          )

    tyGenMconcatVert :: Ass0TypeExpr
    tyGenMconcatVert =
      ("a", tyInt)
        -:> ("b", tyInt)
        -:> ("c", tyInt)
        -:> A0TyCode
          ( ty1Mat (A0Var "a") (A0Var "c")
              ==> ty1Mat (A0Var "b") (A0Var "c")
              ==> ty1Mat (a0add (A0Var "a") (A0Var "b")) (A0Var "c")
          )

    a0add :: Ass0Expr -> Ass0Expr -> Ass0Expr
    a0add a0e1 = A0App (A0App (A0Var "add") a0e1)

tyValInt :: Ass0TypeVal
tyValInt = A0TyValPrim A0TyValInt

-- Makes a closure equipped with `initialEnv`.
clo :: Var -> Ass0TypeVal -> Ass0Expr -> Ass0Val
clo x a0tyv1 a0tye2 = A0ValLam (x, a0tyv1) a0tye2 initialEnv

lam :: Var -> Ass0TypeExpr -> Ass0Expr -> Ass0Expr
lam x a0tye1 = A0Lam (x, a0tye1)

ass0exprVadd :: Int -> Ass0Expr
ass0exprVadd n =
  lam "v1" (ty0Vec n) $
    lam "v2" (ty0Vec n) $
      A0AppBuiltIn (BIVadd n "v1" "v2")

ass0exprVconcat :: Int -> Int -> Ass0Expr
ass0exprVconcat m n =
  lam "v1" (ty0Vec m) $
    lam "v2" (ty0Vec n) $
      A0AppBuiltIn (BIVconcat m n "v1" "v2")

ass0exprMtranspose :: Int -> Int -> Ass0Expr
ass0exprMtranspose m n =
  lam "mat1" (ty0Mat m n) (A0AppBuiltIn (BIMtranspose m n "mat1"))

ass0exprMconcatVert :: Int -> Int -> Int -> Ass0Expr
ass0exprMconcatVert m1 m2 n =
  lam "mat1" (ty0Mat m1 n) $
    lam "mat2" (ty0Mat m2 n) $
      A0AppBuiltIn (BIMconcatVert m1 m2 n "mat1" "mat2")

ass0exprMmult :: Int -> Int -> Int -> Ass0Expr
ass0exprMmult k m n =
  lam "mat1" (ty0Mat k m) $
    lam "mat2" (ty0Mat m n) $
      A0AppBuiltIn (BIMmult k m n "mat1" "mat2")

ass0valBinaryIntArith :: (Var -> Var -> BuiltIn) -> Ass0Val
ass0valBinaryIntArith f =
  clo "x1" tyValInt $
    lam "x2" tyInt $
      A0AppBuiltIn (f "x1" "x2")

ass0valAdd :: Ass0Val
ass0valAdd = ass0valBinaryIntArith BIAdd

ass0valSub :: Ass0Val
ass0valSub = ass0valBinaryIntArith BISub

ass0valMult :: Ass0Val
ass0valMult = ass0valBinaryIntArith BIMult

ass0valGenVadd :: Ass0Val
ass0valGenVadd =
  clo "x1" tyValInt $
    A0AppBuiltIn (BIGenVadd "x1")

ass0valGenVconcat :: Ass0Val
ass0valGenVconcat =
  clo "x1" tyValInt $
    lam "x2" tyInt $
      A0AppBuiltIn (BIGenVconcat "x1" "x2")

ass0valGenMtranspose :: Ass0Val
ass0valGenMtranspose =
  clo "x1" tyValInt $
    lam "x2" tyInt $
      A0AppBuiltIn (BIGenMtranspose "x1" "x2")

ass0valGenMmult :: Ass0Val
ass0valGenMmult =
  clo "x1" tyValInt $
    lam "x2" tyInt $
      lam "x3" tyInt $
        A0AppBuiltIn (BIGenMmult "x1" "x2" "x3")

ass0valGenMconcatVert :: Ass0Val
ass0valGenMconcatVert =
  clo "x1" tyValInt $
    lam "x2" tyInt $
      lam "x3" tyInt $
        A0AppBuiltIn (BIGenMconcatVert "x1" "x2" "x3")

initialEnv :: Env0
initialEnv =
  List.foldl'
    (\env (x, a0v) -> Map.insert x (Ass0ValEntry a0v) env)
    Map.empty
    [ ("add", ass0valAdd),
      ("sub", ass0valSub),
      ("mult", ass0valMult),
      ("gen_vadd", ass0valGenVadd),
      ("gen_vconcat", ass0valGenVconcat),
      ("gen_mtranspose", ass0valGenMtranspose),
      ("gen_mmult", ass0valGenMmult),
      ("gen_mconcat_vert", ass0valGenMconcatVert)
    ]
