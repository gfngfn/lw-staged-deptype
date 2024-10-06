module Util.ParserUtil
  ( GenP,
    expectToken,
    token,
    noLoc,
  )
where

import Data.Set qualified as Set
import Data.Void (Void)
import Text.Megaparsec qualified as Mp
import Util.TokenUtil

type GenP token a = Mp.Parsec Void [Located token] a

expectToken :: (Ord token) => (token -> Maybe a) -> GenP token (Located a)
expectToken f =
  Mp.token
    (\case Located loc t -> fmap (Located loc) (f t))
    Set.empty

token :: (Ord token) => token -> GenP token Span
token tExpected =
  Mp.token
    ( \(Located loc t) ->
        if t == tExpected then Just loc else Nothing
    )
    Set.empty

noLoc :: GenP token (Located a) -> GenP token a
noLoc p = (\(Located _ x) -> x) <$> p
