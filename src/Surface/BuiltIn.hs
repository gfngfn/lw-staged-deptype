module Surface.BuiltIn
  ( initialBindingTimeEnv,
  )
where

import Data.List qualified as List
import Data.Map qualified as Map
import Surface.BindingTimeAnalyzer (BITypeF (..), BITypeMainF (..), BindingTimeConst (..), BindingTimeEnv, BindingTimeEnvEntry (..))
import Surface.Syntax
import Prelude

type BITypeVoid = BITypeF BindingTimeConst

type ExprVoid = ExprF BindingTimeConst

wrap0 :: BITypeMainF BindingTimeConst -> BITypeVoid
wrap0 = BIType BT0

wrap1 :: BITypeMainF BindingTimeConst -> BITypeVoid
wrap1 = BIType BT1

bityInt :: BITypeVoid
bityInt = wrap0 BITyInt

bityVec :: ExprVoid -> BITypeVoid
bityVec = wrap1 . BITyVecExpr

bityMat :: ExprVoid -> ExprVoid -> BITypeVoid
bityMat be1 be2 = wrap1 $ BITyMatExpr (be1, be2)

(-->) :: BITypeVoid -> BITypeVoid -> BITypeVoid
(-->) bity1 bity2 = wrap1 $ BITyArrow (Nothing, bity1) bity2

infixr 0 -->

(-:>) :: (Var, BITypeVoid) -> BITypeVoid -> BITypeVoid
(-:>) (x, bity1) bity2 = wrap0 $ BITyArrow (Just x, bity1) bity2

infixr 0 -:>

initialBindingTimeEnv :: BindingTimeEnv
initialBindingTimeEnv =
  List.foldl'
    (\btenv (x, entry) -> Map.insert x entry btenv)
    Map.empty
    [ ("+", persistent bityArith),
      ("-", persistent bityArith),
      ("*", persistent bityArith),
      ("vadd", fixed0 bityVadd),
      ("vconcat", fixed0 bityVconcat),
      ("mtranspose", fixed0 bityMtranspose),
      ("mmult", fixed0 bityMmult),
      ("mconcat_vert", fixed0 bityMconcatVert)
    ]
  where
    persistent = EntryBuiltInPersistent
    fixed0 = EntryBuiltInFixed BT0

    bityArith :: BITypeF ()
    bityArith =
      int `arrow` (int `arrow` int)
      where
        wrap = BIType ()
        int = wrap BITyInt
        arrow bity1 bity2 = wrap $ BITyArrow (Nothing, bity1) bity2

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
    bVar x = Expr BT0 (Var x)

    bApp :: ExprVoid -> ExprVoid -> ExprVoid
    bApp be1 be2 = Expr BT0 (App be1 be2)
