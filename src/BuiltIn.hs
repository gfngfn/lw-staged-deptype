module BuiltIn
  ( ass0exprVadd,
    ass0exprVconcat,
    initialTypeEnv,
    initialEnv,
  )
where

import Data.List qualified as List
import Data.Map qualified as Map
import Syntax
import TypeEnv (TypeEnv)
import TypeEnv qualified

tyInt :: Ass0TypeExpr
tyInt = A0TyPrim A0TyInt

ty1Vec :: Ass0Expr -> Ass1TypeExpr
ty1Vec = A1TyPrim . A1TyVec

ty0Vec :: Int -> Ass0TypeExpr
ty0Vec = A0TyPrim . A0TyVec

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
      ("gen_vconcat", tyGenVconcat)
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

ass0valAdd :: Ass0Val
ass0valAdd = clo "x1" tyValInt (lam "x2" tyInt (A0AppBuiltIn (BIAdd "x1" "x2")))

ass0valGenVadd :: Ass0Val
ass0valGenVadd = clo "x1" tyValInt (A0AppBuiltIn (BIGenVadd "x1"))

ass0valGenVconcat :: Ass0Val
ass0valGenVconcat = clo "x1" tyValInt (lam "x2" tyInt (A0AppBuiltIn (BIGenVconcat "x1" "x2")))

initialEnv :: Env0
initialEnv =
  List.foldl'
    (\env (x, a0v) -> Map.insert x (Ass0ValEntry a0v) env)
    Map.empty
    [ ("add", ass0valAdd),
      ("gen_vadd", ass0valGenVadd),
      ("gen_vconcat", ass0valGenVconcat)
    ]
