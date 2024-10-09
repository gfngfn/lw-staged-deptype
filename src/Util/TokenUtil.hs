module Util.TokenUtil
  ( Span (..),
    mergeSpan,
    Located (..),
    Tokenizer,
    space,
    lowerIdent,
    upperIdent,
    integerLiteral,
    genLex,
  )
where

import Control.Monad.Combinators
import Data.Char qualified as Char
import Data.Either.Extra qualified as Either
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec qualified as Mp
import Text.Megaparsec.Char qualified as MpChar
import Text.Megaparsec.Char.Lexer qualified as MpLexer
import Prelude

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

type Tokenizer = Mp.Parsec Void Text

space :: Tokenizer ()
space = MpLexer.space MpChar.space1 empty empty

isRestChar :: Char -> Bool
isRestChar c = Char.isAlphaNum c || c == '_'

lowerIdent :: Tokenizer Text
lowerIdent = Text.pack <$> ((:) <$> p1 <*> p2)
  where
    p1 = Mp.satisfy Char.isLower
    p2 = Mp.many (Mp.satisfy isRestChar) <* Mp.notFollowedBy (Mp.satisfy isRestChar)

upperIdent :: Tokenizer Text
upperIdent = Text.pack <$> ((:) <$> p1 <*> p2)
  where
    p1 = Mp.satisfy Char.isUpper
    p2 = Mp.many (Mp.satisfy isRestChar) <* Mp.notFollowedBy (Mp.satisfy isRestChar)

integerLiteral :: Tokenizer Int
integerLiteral = (\s -> read s :: Int) <$> (((:) <$> p1 <*> p2) <|> ((: []) <$> Mp.single '0'))
  where
    p1 = Mp.satisfy (\c -> Char.isDigit c && c /= '0')
    p2 = Mp.many (Mp.satisfy Char.isDigit) <* Mp.notFollowedBy (Mp.satisfy Char.isDigit)

tokenWithOffsets :: Tokenizer token -> Tokenizer (Located token)
tokenWithOffsets getToken = do
  start <- Mp.getOffset
  t <- getToken
  end <- Mp.getOffset
  _ <- space
  pure $ Located (Span start end) t

genLex :: Tokenizer token -> Text -> Either String [Located token]
genLex getToken source =
  Either.mapLeft Mp.errorBundlePretty $
    Mp.parse (space *> manyTill (tokenWithOffsets getToken) Mp.eof) "input" source
