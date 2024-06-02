module MatrixSpec (spec) where

import Lwsd.Matrix (Matrix)
import Lwsd.Matrix qualified as Matrix
import Test.Hspec

make :: [[Int]] -> Matrix
make rows =
  case Matrix.fromList rows of
    Left _ -> error "make"
    Right mat -> mat

spec :: Spec
spec = do
  describe "Matrix.transpose" $ do
    it "computes transpositions (1)" $
      Matrix.transpose (make [[1, 2], [3, 4]])
        `shouldBe` make [[1, 3], [2, 4]]
    it "computes transpositions (2)" $
      Matrix.transpose (make [[1, 2, 3], [4, 5, 6]])
        `shouldBe` make [[1, 4], [2, 5], [3, 6]]
  describe "Matrix.mult" $ do
    it "computes multiplications" $
      Matrix.mult 2 3 1
        (make [[1, 2, 3], [4, 5, 6]])
        (make [[8], [9], [10]])
          `shouldBe` Just (make [[56, 137]])
    -- 1*8 + 2*9 + 3*10 = 56
    -- 4*8 + 5*9 + 6*10 = 137
