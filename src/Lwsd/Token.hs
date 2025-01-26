module Lwsd.Token
  ( Token (..),
    lex,
  )
where

import Control.Monad.Combinators
import Data.Either.Extra
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Base
import GHC.Generics
import Text.Megaparsec qualified as Mp
import Util.TokenUtil
import Prelude hiding (lex)

data Token
  = TokLeftParen
  | TokRightParen
  | TokLeftBrace
  | TokRightBrace
  | TokLeftSquare
  | TokRightSquare
  | TokArrow
  | TokEqual
  | TokColon
  | TokComma
  | TokBracket
  | TokEscape
  | TokPersistent
  | TokSemicolon
  | TokUnderscore
  | TokVecLeft
  | TokVecRight
  | TokMatLeft
  | TokMatRight
  | TokLower Text
  | TokUpper Text
  | TokLongLower ([Text], Text)
  | TokInt Int
  | TokFloat Double
  | TokString Text
  | TokFun
  | TokRec
  | TokLet
  | TokIn
  | TokIf
  | TokThen
  | TokElse
  | TokAs
  | TokVal
  | TokModule
  | TokSig
  | TokEnd
  | TokExternal
  | TokOpen
  | TokOpAdd Text
  | TokOpMult Text
  | TokOpComp Text
  deriving stock (Ord, Eq, Show, Generic)

instance Mp.VisualStream [Located Token] where
  showTokens _proxy = show

instance Mp.TraversableStream [Located Token] where
  reachOffset _n posState = (Nothing, posState)

keywordMap :: Map Text Token
keywordMap =
  Map.fromList
    [ ("fun", TokFun),
      ("rec", TokRec),
      ("let", TokLet),
      ("in", TokIn),
      ("if", TokIf),
      ("then", TokThen),
      ("else", TokElse),
      ("as", TokAs),
      ("val", TokVal),
      ("module", TokModule),
      ("sig", TokSig),
      ("end", TokEnd),
      ("external", TokExternal),
      ("open", TokOpen)
    ]

lowerIdentOrKeyword :: Tokenizer Token
lowerIdentOrKeyword = do
  t <- lowerIdent
  pure $ case Map.lookup t keywordMap of
    Just tok -> tok
    Nothing -> TokLower t

opRestCharSet :: Set Char
opRestCharSet =
  Set.fromList ['+', '-', '*', '/', '=', '<', '>']

opRestChar :: Tokenizer Char
opRestChar =
  Mp.satisfy (`elem` opRestCharSet)

operator :: Char -> Tokenizer Text
operator firstChar =
  Text.pack <$> ((:) <$> Mp.single firstChar <*> (Mp.many opRestChar <* Mp.notFollowedBy opRestChar))

token :: Tokenizer Token
token =
  choice
    [ TokLeftParen <$ Mp.single '(',
      TokRightParen <$ Mp.single ')',
      TokLeftBrace <$ Mp.single '{',
      TokRightBrace <$ Mp.single '}',
      TokArrow <$ Mp.chunk "->",
      TokColon <$ Mp.single ':',
      TokComma <$ Mp.single ',',
      TokEqual <$ Mp.single '=',
      TokBracket <$ Mp.single '&',
      TokEscape <$ Mp.single '~',
      TokPersistent <$ Mp.single '%',
      TokSemicolon <$ Mp.single ';',
      TokUnderscore <$ Mp.single '_',
      TokVecLeft <$ Mp.chunk "[|",
      TokVecRight <$ Mp.chunk "|]",
      TokMatLeft <$ Mp.chunk "[#",
      TokMatRight <$ Mp.chunk "#]",
      TokLeftSquare <$ Mp.single '[',
      TokRightSquare <$ Mp.single ']',
      TokOpAdd <$> operator '+',
      TokOpAdd <$> operator '-',
      TokOpMult <$> operator '*',
      TokOpMult <$> operator '/',
      TokOpComp <$> operator '=',
      TokOpComp <$> operator '<',
      TokOpComp <$> operator '>',
      lowerIdentOrKeyword,
      Mp.try (TokLongLower <$> longLowerIdent),
      TokUpper <$> upperIdent,
      Mp.try (TokFloat <$> floatLiteral),
      TokInt <$> integerLiteral,
      TokString <$> stringLiteral
    ]

lex :: Text -> Either String [Located Token]
lex = genLex token
