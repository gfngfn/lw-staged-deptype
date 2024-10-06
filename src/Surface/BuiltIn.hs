module Surface.BuiltIn
  ( initialBindingTimeEnv,
  )
where

import Data.List qualified as List
import Data.Map qualified as Map
import Surface.BindingTimeAnalyzer (BITypeF (..), BITypeMainF (..), BITypeVoid, BindingTimeEnv, BindingTimeEnvEntry (..), ExprVoid)
import Surface.Syntax
import Prelude

wrapBIType :: BITypeMainF () -> BITypeVoid
wrapBIType = BIType ()

bityInt :: BITypeVoid
bityInt = wrapBIType BITyInt

bityVec :: ExprVoid -> BITypeVoid
bityVec = wrapBIType . BITyVecExpr

bityMat :: ExprVoid -> ExprVoid -> BITypeVoid
bityMat be1 be2 = wrapBIType $ BITyMatExpr (be1, be2)

(-->) :: BITypeVoid -> BITypeVoid -> BITypeVoid
(-->) bity1 bity2 = wrapBIType $ BITyArrow (Nothing, bity1) bity2

infixr 0 -->

(-:>) :: (Var, BITypeVoid) -> BITypeVoid -> BITypeVoid
(-:>) (x, bity1) bity2 = wrapBIType $ BITyArrow (Just x, bity1) bity2

infixr 0 -:>

initialBindingTimeEnv :: BindingTimeEnv
initialBindingTimeEnv =
  List.foldl'
    (\btenv (x, bity) -> Map.insert x (EntryBuiltIn bity) btenv)
    Map.empty
    [ ("+", bityInt --> bityInt --> bityInt),
      ("-", bityInt --> bityInt --> bityInt),
      ("*", bityInt --> bityInt --> bityInt),
      ("vadd", bityVadd),
      ("vconcat", bityVconcat),
      ("mtranspose", bityMtranspose),
      ("mmult", bityMmult),
      ("mconcat_vert", bityMconcatVert)
    ]
  where
    bityVadd :: BITypeVoid
    bityVadd =
      ("a", bityInt)
        -:> bityVec (bVar "a")
        --> bityVec (bVar "a")
        --> bityVec (bVar "a")

    bityVconcat :: BITypeVoid
    bityVconcat =
      ("a", bityInt)
        -:> ("b", bityInt)
        -:> bityVec (bVar "a")
        --> bityVec (bVar "b")
        --> bityVec (bAdd (bVar "a") (bVar "b"))

    bityMtranspose :: BITypeVoid
    bityMtranspose =
      ("a", bityInt)
        -:> ("b", bityInt)
        -:> bityMat (bVar "a") (bVar "b")
        --> bityMat (bVar "b") (bVar "a")

    bityMmult :: BITypeVoid
    bityMmult =
      ("a", bityInt)
        -:> ("b", bityInt)
        -:> ("c", bityInt)
        -:> bityMat (bVar "a") (bVar "b")
        --> bityMat (bVar "b") (bVar "c")
        --> bityMat (bVar "a") (bVar "c")

    bityMconcatVert :: BITypeVoid
    bityMconcatVert =
      ("a", bityInt)
        -:> ("b", bityInt)
        -:> ("c", bityInt)
        -:> bityMat (bVar "a") (bVar "c")
        --> bityMat (bVar "b") (bVar "c")
        --> bityMat (bAdd (bVar "a") (bVar "b")) (bVar "c")

    bAdd :: ExprVoid -> ExprVoid -> ExprVoid
    bAdd be1 = bApp (bApp (bVar "+") be1)

    bVar :: Var -> ExprVoid
    bVar x = Expr () (Var x)

    bApp :: ExprVoid -> ExprVoid -> ExprVoid
    bApp be1 be2 = Expr () (App be1 be2)
