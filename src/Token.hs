module Token (Token (..), lex) where

import Control.Monad.Combinators
import Data.Char qualified as Char
import Data.Either.Extra
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import GHC.Base
import Text.Megaparsec qualified as Mp
import Text.Megaparsec.Char qualified as MpChar
import Text.Megaparsec.Char.Lexer qualified as MpLexer
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
  | TokLower Text
  | TokUpper Text
  | TokFun
  | TokLet
  | TokIn
  deriving stock (Ord, Eq, Show)

instance Mp.VisualStream [Token] where
  showTokens _proxy = show

instance Mp.TraversableStream [Token] where
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
      lowerIdentOrKeyword,
      TokUpper <$> upperIdent
    ]

lex :: Text -> Either String [Token]
lex source =
  mapLeft Mp.errorBundlePretty $
    Mp.parse (space *> manyTill (token <* space) Mp.eof) "input" source
