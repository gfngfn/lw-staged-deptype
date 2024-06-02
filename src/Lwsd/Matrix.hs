module Lwsd.Matrix
  ( Matrix,
    ConstructionError (..),
    fromList,
    toList,
    size,
    transpose,
    mult,
  )
where

import Data.List qualified as List
import Prelude

newtype Matrix = Matrix [[Int]]
  deriving newtype (Eq, Show)

data ConstructionError = ConstructionError
  { row1 :: [Int],
    row2 :: [Int]
  }

fromList :: [[Int]] -> Either ConstructionError Matrix
fromList [] = pure $ Matrix []
fromList rows@(firstRow : restRows) =
  case List.find (\row -> List.length row /= expectedLength) restRows of
    Just row' -> Left $ ConstructionError firstRow row'
    Nothing -> pure $ Matrix rows
  where
    expectedLength = List.length firstRow

toList :: Matrix -> [[Int]]
toList (Matrix rows) = rows

size :: Matrix -> (Int, Int)
size (Matrix []) = (0, 0)
size (Matrix rows@(firstRow : _)) = (List.length rows, List.length firstRow)

transpose :: Matrix -> Matrix
transpose (Matrix []) = Matrix []
transpose (Matrix (firstRow : restRows)) =
  Matrix $ reverse $ go [] firstRow restRows
  where
    go acc [] restRows0 =
      if List.all null restRows0
        then acc
        else error "transpose 1"
    go acc (firstElem : restElems) restRows0 =
      let pairs =
            map
              ( \case
                  [] -> error "transpose 2"
                  firstElem0 : restElems0 -> (firstElem0, restElems0)
              )
              restRows0
          column = firstElem : map fst pairs
          restRows1 = map snd pairs
       in go (column : acc) restElems restRows1

mult :: Int -> Int -> Int -> Matrix -> Matrix -> Maybe Matrix
mult k m n mat1 mat2 =
  if size mat1 == (k, m) || size mat2 == (m, n)
  -- `mat1`: a matrix of size k × m
  -- `mat2`: a matrix of size m × n
    then Just $ Matrix [ [ calc row1 column2 | row1 <- rows1 ] | column2 <- columns2 ]
    else Nothing
  where
    Matrix rows1 = mat1 -- `rows1`: a list of length k
    Matrix columns2 = transpose mat2 -- `columns2`: a list of length n

    calc :: [Int] -> [Int] -> Int
    calc row column =
      List.foldl' (+) 0 $ zipWith (*) row column
