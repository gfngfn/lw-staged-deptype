module Util.ParserUtil
  ( GenP,
    runParser,
    failure,
    eof,
    some,
    many,
    manyNoTry,
    sepBy,
    tries,
    or,
    expectToken,
    token,
    noLoc,
    binSep,
    genVec,
    genMat,
  )
where

import Data.Either.Extra qualified as Either
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set qualified as Set
import Data.Void (Void)
import Text.Megaparsec ((<|>))
import Text.Megaparsec qualified as Mp
import Util.TokenUtil
import Prelude hiding (or)

type GenP token a = Mp.Parsec Void [Located token] a

runParser :: (Ord token, Mp.VisualStream [Located token], Mp.TraversableStream [Located token]) => GenP token a -> [Located token] -> Either String a
runParser p locatedTokens =
  Either.mapLeft Mp.errorBundlePretty $ Mp.parse p "input" locatedTokens

eof :: (Ord token) => GenP token ()
eof = Mp.eof

failure :: (Ord token) => Located token -> GenP token a
failure unexpectedToken =
  Mp.failure (Just (Mp.Tokens (unexpectedToken :| []))) Set.empty

some :: (Ord token) => GenP token a -> GenP token (NonEmpty a)
some p = do
  xs <- Mp.some (Mp.try p)
  case xs of
    [] -> error "bug: Text.Megaparsec.some returned the empty list"
    x : xs' -> pure (x :| xs')

many :: (Ord token) => GenP token a -> GenP token [a]
many = Mp.many . Mp.try

manyNoTry :: (Ord token) => GenP token a -> GenP token [a]
manyNoTry = Mp.many

sepBy :: (Ord token) => GenP token a -> GenP token sep -> GenP token [a]
sepBy = Mp.sepBy

tries :: (Ord token) => [GenP token a] -> GenP token a -> GenP token a
tries ps pAcc0 = foldr or pAcc0 ps

or :: (Ord token) => GenP token a -> GenP token a -> GenP token a
or p1 p2 = Mp.try p1 <|> p2

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

binSep :: (Ord token) => (a -> op -> a -> a) -> GenP token op -> GenP token a -> GenP token a
binSep k pBinOp pEntry =
  List.foldl' (\e1 (locBinOp, e2) -> k e1 locBinOp e2) <$> pEntry <*> many ((,) <$> pBinOp <*> pEntry)

genVec :: (Ord token) => token -> token -> token -> GenP token entry -> GenP token (Located [entry])
genVec tLeft tRight tSemicolon entry = makeVec <$> token tLeft <*> rest
  where
    rest =
      (makeNonemptyVec <$> entry <*> many (token tSemicolon *> entry) <*> token tRight)
        `or` (([],) <$> token tRight)

    makeNonemptyVec elemFirst elemsTail locLast =
      (elemFirst : elemsTail, locLast)

    makeVec locFirst (elems, locLast) =
      Located (mergeSpan locFirst locLast) elems

genMat :: (Ord token) => token -> token -> token -> token -> GenP token entry -> GenP token (Located [[entry]])
genMat tLeft tRight tSemicolon tComma entry = makeMat <$> token tLeft <*> rest
  where
    rest =
      (makeNonemptyMat <$> nonemptyRow <*> many (token tSemicolon *> nonemptyRow) <*> token tRight)
        `or` (([],) <$> token tRight)

    makeNonemptyMat rowFirst rowsTail locLast =
      (rowFirst : rowsTail, locLast)

    makeMat locFirst (rows, locLast) =
      Located (mergeSpan locFirst locLast) rows

    nonemptyRow =
      (:) <$> entry <*> many (token tComma *> entry)
