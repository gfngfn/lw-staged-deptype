module Util.ParserUtil
  ( Span (..),
    mergeSpan,
    Located (..),
    GenP,
    token,
  )
where

import Data.Set qualified as Set
import Data.Void (Void)
import Text.Megaparsec qualified as Mp

-- The type for code locations (pairs of a start offset and an end offset).
data Span = Span
  { start :: Int,
    end :: Int
  }
  deriving (Eq, Ord, Show)

mergeSpan :: Span -> Span -> Span
mergeSpan (Span {start}) (Span {end}) = Span {start, end}

data Located a = Located Span a
  deriving (Eq, Ord, Show, Functor)

type GenP token a = Mp.Parsec Void [Located token] a

token :: (Ord token) => token -> GenP token Span
token tExpected =
  Mp.token
    ( \(Located loc t) ->
        if t == tExpected then Just loc else Nothing
    )
    Set.empty
