module ConstraintSpec (spec) where

import Data.Map qualified as Map
import Data.Tuple.Extra qualified as Tuple
import Surface.BindingTime.Constraint
import Surface.BindingTime.Core
import Test.Hspec

v :: Int -> BindingTime
v n = BTVar (BindingTimeVar n)

leq :: BindingTime -> BindingTime -> Constraint ()
leq = CLeq ()

equ :: BindingTime -> BindingTime -> Constraint ()
equ = CEqual ()

const0, const1 :: BindingTime
const0 = BTConst BT0
const1 = BTConst BT1

ok :: [(Int, BindingTime)] -> [Constraint ann] -> Either ann (BindingTimeSubst, [Constraint ann])
ok substs unsolved = Right (Map.fromList (map (Tuple.first BindingTimeVar) substs), unsolved)

spec :: Spec
spec = do
  describe "Surface.BindingTime.Constraint.solveConstraints" $ do
    it "leq-and-eq (1)" $
      solveConstraints [leq (v 1) (v 2), equ (v 1) const1]
        `shouldBe` ok [(1, const1), (2, const1)] []
    it "leq-and-eq (2)" $
      solveConstraints [leq (v 1) (v 2), equ (v 2) const0]
        `shouldBe` ok [(1, const0), (2, const0)] []
