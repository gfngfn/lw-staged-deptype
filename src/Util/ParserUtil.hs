module Util.ParserUtil
  ( GenP,
    expectToken,
    token,
    noLoc,
    genVec,
    genMat,
  )
where

import Data.Set qualified as Set
import Data.Void (Void)
import Text.Megaparsec ((<|>))
import Text.Megaparsec qualified as Mp
import Util.TokenUtil
import Prelude

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

genVec :: (Ord token) => token -> token -> token -> GenP token entry -> GenP token (Located [entry])
genVec tLeft tRight tSemicolon entry = makeVec <$> token tLeft <*> rest
  where
    rest =
      Mp.try (makeNonemptyVec <$> entry <*> Mp.many (token tSemicolon *> entry) <*> token tRight)
        <|> (([],) <$> token tRight)

    makeNonemptyVec elemFirst elemsTail locLast =
      (elemFirst : elemsTail, locLast)

    makeVec locFirst (elems, locLast) =
      Located (mergeSpan locFirst locLast) elems

genMat :: (Ord token) => token -> token -> token -> token -> GenP token entry -> GenP token (Located [[entry]])
genMat tLeft tRight tSemicolon tComma entry = makeMat <$> token tLeft <*> rest
  where
    rest =
      Mp.try (makeNonemptyMat <$> nonemptyRow <*> Mp.many (token tSemicolon *> nonemptyRow) <*> token tRight)
        <|> (([],) <$> token tRight)

    makeNonemptyMat rowFirst rowsTail locLast =
      (rowFirst : rowsTail, locLast)

    makeMat locFirst (rows, locLast) =
      Located (mergeSpan locFirst locLast) rows

    nonemptyRow =
      (:) <$> entry <*> Mp.many (token tComma *> entry)
