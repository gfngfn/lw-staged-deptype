module Lwsd.Token
  ( Token (..),
    lex,
  )
where

import Control.Monad.Combinators
import Data.Either.Extra
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
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
  | TokOpFlipApp
  | TokBar
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
  | TokStruct
  | TokEnd
  | TokExternal
  | TokOpen
  | TokOpAdd Text
  | TokOpMult Text
  | TokOpComp Text
  deriving stock (Ord, Eq, Show, Generic)

instance Mp.VisualStream [Located Token] where
  showTokens _proxy tokens =
    unwords (map (\(Located _ t) -> showToken t) (NonEmpty.toList tokens))

showToken :: Token -> String
showToken = \case
  TokLeftParen -> "("
  TokRightParen -> ")"
  TokLeftBrace -> "{"
  TokRightBrace -> "}"
  TokLeftSquare -> "["
  TokRightSquare -> "]"
  TokArrow -> "->"
  TokEqual -> "="
  TokColon -> ":"
  TokComma -> ","
  TokBracket -> "&"
  TokEscape -> "~"
  TokPersistent -> "%"
  TokSemicolon -> ";"
  TokUnderscore -> "_"
  TokOpFlipApp -> "|>"
  TokBar -> "|"
  TokVecLeft -> "[|"
  TokVecRight -> "|]"
  TokMatLeft -> "[#"
  TokMatRight -> "#]"
  TokLower lower -> Text.unpack lower
  TokUpper upper -> Text.unpack upper
  TokLongLower (mods, lower) -> Text.unpack (Text.intercalate "." mods <> lower)
  TokInt n -> show n
  TokFloat r -> show r
  TokString s -> show s
  TokFun -> "fun"
  TokRec -> "rec"
  TokLet -> "let"
  TokIn -> "in"
  TokIf -> "if"
  TokThen -> "then"
  TokElse -> "else"
  TokAs -> "as"
  TokVal -> "val"
  TokModule -> "module"
  TokStruct -> "struct"
  TokEnd -> "end"
  TokExternal -> "external"
  TokOpen -> "open"
  TokOpAdd op -> Text.unpack op
  TokOpMult op -> Text.unpack op
  TokOpComp op -> Text.unpack op

instance Mp.TraversableStream [Located Token] where
  reachOffset _n posState = (Nothing, posState) -- TODO

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
      ("struct", TokStruct),
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
      TokOpFlipApp <$ Mp.chunk "|>",
      TokVecLeft <$ Mp.chunk "[|",
      TokVecRight <$ Mp.chunk "|]",
      TokMatLeft <$ Mp.chunk "[#",
      TokMatRight <$ Mp.chunk "#]",
      TokLeftSquare <$ Mp.single '[',
      TokRightSquare <$ Mp.single ']',
      TokBar <$ Mp.single '|',
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
lex = genLex token comment
