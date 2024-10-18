module SubstSpec (spec) where

import Lwsd.Subst
import SyntaxUtil
import Test.Hspec

spec :: Spec
spec = do
  describe "subst0" $ do
    it "substitutes variables (match)" $
      subst0 (a0litInt 42) "foo" (a0var "foo")
        `shouldBe` (a0litInt 42)
    it "substitutes variables (ignore)" $
      subst0 (a0litInt 42) "foo" (a0var "bar")
        `shouldBe` (a0var "bar")
    it "ignores stage-1 variables" $
      subst0 (a0litInt 42) "foo" (a0bracket (a1var "foo"))
        `shouldBe` (a0bracket (a1var "foo"))
