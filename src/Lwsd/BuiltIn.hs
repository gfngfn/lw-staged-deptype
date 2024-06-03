module Lwsd.BuiltIn
  ( ass0exprVadd,
    ass0exprVconcat,
    ass0exprMtranspose,
    ass0exprMmult,
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
      ("gen_vadd", tyGenVadd),
      ("gen_vconcat", tyGenVconcat),
      ("gen_mtranspose", tyGenMtranspose),
      ("gen_mmult", tyGenMmult)
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
              ==> ty1Vec (A0App (A0App (A0Var "add") (A0Var "a")) (A0Var "b"))
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

tyValInt :: Ass0TypeVal
tyValInt = A0TyValPrim A0TyValInt

-- Makes a closure equipped with `initialEnv`.
clo :: Var -> Ass0TypeVal -> Ass0Expr -> Ass0Val
clo x a0tyv1 a0tye2 = A0ValLam (x, a0tyv1) a0tye2 initialEnv

lam :: Var -> Ass0TypeExpr -> Ass0Expr -> Ass0Expr
lam x a0tye1 = A0Lam (x, a0tye1)

ass0exprVadd :: Int -> Ass0Expr
ass0exprVadd n = lam "v1" (ty0Vec n) (lam "v2" (ty0Vec n) (A0AppBuiltIn (BIVadd n "v1" "v2")))

ass0exprVconcat :: Int -> Int -> Ass0Expr
ass0exprVconcat m n = lam "v1" (ty0Vec m) (lam "v2" (ty0Vec n) (A0AppBuiltIn (BIVconcat m n "v1" "v2")))

ass0exprMtranspose :: Int -> Int -> Ass0Expr
ass0exprMtranspose m n = lam "m1" (ty0Mat m n) (A0AppBuiltIn (BIMtranspose m n "m1"))

ass0exprMmult :: Int -> Int -> Int -> Ass0Expr
ass0exprMmult k m n = lam "m1" (ty0Mat k m) (lam "m2" (ty0Mat m n) (A0AppBuiltIn (BIMmult k m n "m1" "m2")))

ass0valAdd :: Ass0Val
ass0valAdd = clo "x1" tyValInt (lam "x2" tyInt (A0AppBuiltIn (BIAdd "x1" "x2")))

ass0valGenVadd :: Ass0Val
ass0valGenVadd = clo "x1" tyValInt (A0AppBuiltIn (BIGenVadd "x1"))

ass0valGenVconcat :: Ass0Val
ass0valGenVconcat = clo "x1" tyValInt (lam "x2" tyInt (A0AppBuiltIn (BIGenVconcat "x1" "x2")))

ass0valGenMtranspose :: Ass0Val
ass0valGenMtranspose = clo "x1" tyValInt (lam "x2" tyInt (A0AppBuiltIn (BIGenMtranspose "x1" "x2")))

ass0valGenMmult :: Ass0Val
ass0valGenMmult = clo "x1" tyValInt (lam "x2" tyInt (lam "x3" tyInt (A0AppBuiltIn (BIGenMmult "x1" "x2" "x3"))))

initialEnv :: Env0
initialEnv =
  List.foldl'
    (\env (x, a0v) -> Map.insert x (Ass0ValEntry a0v) env)
    Map.empty
    [ ("add", ass0valAdd),
      ("gen_vadd", ass0valGenVadd),
      ("gen_vconcat", ass0valGenVconcat),
      ("gen_mtranspose", ass0valGenMtranspose),
      ("gen_mmult", ass0valGenMmult)
    ]
