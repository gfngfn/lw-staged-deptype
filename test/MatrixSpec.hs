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
