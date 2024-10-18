module Surface.Token
  ( Token (..),
    lex,
  )
where

import Control.Monad.Combinators
import Data.Either.Extra
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Base
import GHC.Generics
import Text.Megaparsec qualified as Mp
import Util.TokenUtil
import Prelude hiding (lex)

data Token
  = TokLeftParen
  | TokRightParen
  | TokArrow
  | TokEqual
  | TokColon
  | TokComma
  | TokSemicolon
  | TokVecLeft
  | TokVecRight
  | TokMatLeft
  | TokMatRight
  | TokLower Text
  | TokUpper Text
  | TokInt Int
  | TokFun
  | TokLet
  | TokIn
  | TokIf
  | TokThen
  | TokElse
  | TokOpAdd
  | TokOpSub
  | TokOpMult
  deriving stock (Ord, Eq, Show, Generic)

instance Mp.VisualStream [Located Token] where
  showTokens _proxy = show

instance Mp.TraversableStream [Located Token] where
  reachOffset _n posState = (Nothing, posState)

keywordMap :: Map Text Token
keywordMap =
  Map.fromList
    [ ("fun", TokFun),
      ("let", TokLet),
      ("in", TokIn),
      ("if", TokIf),
      ("then", TokThen),
      ("else", TokElse)
    ]

lowerIdentOrKeyword :: Tokenizer Token
lowerIdentOrKeyword = do
  t <- lowerIdent
  pure $ case Map.lookup t keywordMap of
    Just tok -> tok
    Nothing -> TokLower t

token :: Tokenizer Token
token =
  choice
    [ TokLeftParen <$ Mp.single '(',
      TokRightParen <$ Mp.single ')',
      TokArrow <$ Mp.chunk "->",
      TokColon <$ Mp.single ':',
      TokComma <$ Mp.single ',',
      TokEqual <$ Mp.single '=',
      TokSemicolon <$ Mp.single ';',
      TokVecLeft <$ Mp.chunk "[|",
      TokVecRight <$ Mp.chunk "|]",
      TokMatLeft <$ Mp.chunk "[#",
      TokMatRight <$ Mp.chunk "#]",
      TokOpAdd <$ Mp.single '+',
      TokOpSub <$ Mp.single '-',
      TokOpMult <$ Mp.single '*',
      lowerIdentOrKeyword,
      TokUpper <$> upperIdent,
      TokInt <$> integerLiteral
    ]

lex :: Text -> Either String [Located Token]
lex = genLex token
