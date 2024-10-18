module SubstSpec (spec) where

import Data.Set qualified as Set
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
  describe "frees" $ do
    it "ignores stage-0 bound variables (non-rec)" $
      frees (a0nonrecLam "x" a0tyInt (a0app (a0app (a0var "f") (a0var "x")) (a0bracket (a1var "y"))))
        `shouldBe` (Set.fromList ["f"], Set.fromList ["y"])
    it "ignores stage-0 bound variables (rec)" $
      frees (a0recLam "f" (a0nondepTyArrow a0tyInt a0tyInt) "x" a0tyInt (a0app (a0var "f") (a0var "x")))
        `shouldBe` (Set.fromList [], Set.fromList [])
    it "takes type annotations into account" $
      frees (a1nonrecLam "v" (a1tyVec (a0var "n")) (a1escape (a0app (a0var "f") (a0bracket (a1var "v")))))
        `shouldBe` (Set.fromList ["f", "n"], Set.fromList [])
    it "ignores stage-1 bound variables (non-rec)" $
      frees (a1nonrecLam "x" a1tyInt (a1app (a1app (a1var "f") (a1var "x")) (a1escape (a0var "y"))))
        `shouldBe` (Set.fromList ["y"], Set.fromList ["f"])
  describe "alphaEquivalent" $ do
    it "judges alpha-equivalence of functions (1)" $
      alphaEquivalent
        (a0nonrecLam "foo" a0tyInt (a0var "foo"))
        (a0nonrecLam "bar" a0tyInt (a0var "bar"))
        `shouldBe` True
    it "judges alpha-equivalence of functions (2)" $
      alphaEquivalent
        (a0nonrecLam "foo" a0tyInt (a0var "bar"))
        (a0nonrecLam "bar" a0tyInt (a0var "bar"))
        `shouldBe` False
