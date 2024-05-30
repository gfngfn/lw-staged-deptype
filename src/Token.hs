module Token
  ( Token (..),
    Span (..),
    mergeSpan,
    Located (..),
    lex,
    LocationInFile (..),
    getLocationInFileFromOffset,
  )
where

import Control.Monad.Combinators
import Data.Char qualified as Char
import Data.Either.Extra
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import GHC.Base
import Text.Megaparsec (PosState (..), SourcePos (..))
import Text.Megaparsec qualified as Mp
import Text.Megaparsec.Char qualified as MpChar
import Text.Megaparsec.Char.Lexer qualified as MpLexer
import Text.Megaparsec.Pos qualified as MpPos
import Text.Megaparsec.Stream qualified as MpStream
import Prelude hiding (lex)

data Token
  = TokLeftParen
  | TokRightParen
  | TokArrow
  | TokEqual
  | TokColon
  | TokBracket
  | TokEscape
  | TokPersistent
  | TokSemicolon
  | TokVecLeft
  | TokVecRight
  | TokLower Text
  | TokUpper Text
  | TokInt Int
  | TokFun
  | TokLet
  | TokIn
  deriving stock (Ord, Eq, Show)

instance Mp.VisualStream [Located Token] where
  showTokens _proxy = show

instance Mp.TraversableStream [Located Token] where
  reachOffset _n posState = (Nothing, posState)

type Tokenizer = Mp.Parsec Void Text

keywordMap :: Map Text Token
keywordMap =
  Map.fromList
    [ ("fun", TokFun),
      ("let", TokLet),
      ("in", TokIn)
    ]

space :: Tokenizer ()
space = MpLexer.space MpChar.space1 empty empty

isRestChar :: Char -> Bool
isRestChar c = Char.isAlphaNum c || c == '_'

lowerIdent :: Tokenizer Text
lowerIdent = Text.pack <$> ((:) <$> p1 <*> p2)
  where
    p1 = Mp.satisfy Char.isLower
    p2 = Mp.many (Mp.satisfy isRestChar) <* Mp.notFollowedBy (Mp.satisfy isRestChar)

lowerIdentOrKeyword :: Tokenizer Token
lowerIdentOrKeyword = do
  t <- lowerIdent
  pure $ case Map.lookup t keywordMap of
    Just tok -> tok
    Nothing -> TokLower t

upperIdent :: Tokenizer Text
upperIdent = Text.pack <$> ((:) <$> p1 <*> p2)
  where
    p1 = Mp.satisfy Char.isUpper
    p2 = Mp.many (Mp.satisfy isRestChar) <* Mp.notFollowedBy (Mp.satisfy isRestChar)

integerLiteral :: Tokenizer Int
integerLiteral = (\s -> read s :: Int) <$> ((:) <$> p1 <*> p2)
  where
    p1 = Mp.satisfy (\c -> Char.isDigit c && c /= '0')
    p2 = Mp.many (Mp.satisfy Char.isDigit) <* Mp.notFollowedBy (Mp.satisfy Char.isDigit)

token :: Tokenizer Token
token =
  choice
    [ TokLeftParen <$ Mp.single '(',
      TokRightParen <$ Mp.single ')',
      TokArrow <$ Mp.chunk "->",
      TokColon <$ Mp.single ':',
      TokEqual <$ Mp.single '=',
      TokBracket <$ Mp.single '&',
      TokEscape <$ Mp.single '~',
      TokPersistent <$ Mp.single '%',
      TokSemicolon <$ Mp.single ';',
      TokVecLeft <$ Mp.chunk "[|",
      TokVecRight <$ Mp.chunk "|]",
      lowerIdentOrKeyword,
      TokUpper <$> upperIdent,
      TokInt <$> integerLiteral
    ]

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

tokenWithOffsets :: Tokenizer (Located Token)
tokenWithOffsets = do
  start <- Mp.getOffset
  t <- token
  end <- Mp.getOffset
  _ <- space
  pure $ Located (Span start end) t

lex :: Text -> Either String [Located Token]
lex source =
  mapLeft Mp.errorBundlePretty $
    Mp.parse (space *> manyTill tokenWithOffsets Mp.eof) "input" source

data LocationInFile = LocationInFile
  { line :: Int,
    column :: Int
  }
  deriving stock (Eq, Show)

getLocationInFileFromOffset :: String -> Text -> Int -> (LocationInFile, Maybe String)
getLocationInFileFromOffset inputFilePath source offset =
  let
    initialState =
      PosState
        { pstateInput = source,
          pstateOffset = 0,
          pstateSourcePos = MpPos.initialPos inputFilePath,
          pstateTabWidth = MpPos.defaultTabWidth,
          pstateLinePrefix = ""
        }
    (maybeLineText, finalState) = MpStream.reachOffset offset initialState
    PosState {pstateSourcePos = finalPos} = finalState
    SourcePos {sourceLine, sourceColumn} = finalPos
    locInFile =
      LocationInFile
        { line = MpPos.unPos sourceLine,
          column = MpPos.unPos sourceColumn
        }
  in
  (locInFile, maybeLineText)
