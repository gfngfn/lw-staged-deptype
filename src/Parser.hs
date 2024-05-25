module Parser
  ( parseExpr,
    parseTypeExpr,
  )
where

import Data.Either.Extra
import Data.Functor
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Void (Void)
import Syntax
import Text.Megaparsec hiding (Token, parse, token, tokens)
import Text.Megaparsec qualified as Mp
import Token (Token (..))
import Token qualified
import Prelude hiding (mod)
import Vector qualified

type P = Mp.Parsec Void [Token]

token :: Token -> P ()
token t = void $ Mp.single t

paren :: P a -> P a
paren p = token TokLeftParen *> p <* token TokRightParen

lower :: P Text
lower =
  Mp.token
    ( \case
        TokLower x -> Just x
        _ -> Nothing
    )
    Set.empty

upper :: P Text
upper =
  Mp.token
    ( \case
        TokUpper x -> Just x
        _ -> Nothing
    )
    Set.empty

int :: P Int
int =
  Mp.token
    ( \case
        TokInt n -> Just n
        _ -> Nothing
    )
    Set.empty

vec :: P [Int]
vec = token TokVecLeft *> (try (nonempty <* token TokVecRight) <|> ([] <$ token TokVecRight))
  where
    nonempty = (:) <$> int <*> Mp.many (token TokSemicolon *> int)

exprAtom, expr :: P Expr
(exprAtom, expr) = (atom, letin)
  where
    atom =
      try (Literal . LitInt <$> int)
        <|> try (Literal . LitVec . Vector.fromList <$> vec)
        <|> try (Var <$> lower)
        <|> paren expr
    staged =
      try (Bracket <$> (token TokBracket *> staged))
        <|> try (Escape <$> (token TokEscape *> staged))
        <|> atom
    app =
      foldl1 App <$> Mp.some (try staged)
    lam =
      try (Lam <$> (token TokFun *> paren ((,) <$> lower <*> (token TokColon *> typeExpr)) <* token TokArrow) <*> expr)
        <|> app
    letin =
      try (LetIn <$> (token TokLet *> lower) <*> (token TokEqual *> letin) <*> (token TokIn *> letin))
        <|> lam

typeExpr :: P TypeExpr
typeExpr = fun
  where
    atom =
      try (flip TyName [] <$> upper)
        <|> paren fun
    app =
      try (TyName <$> upper <*> Mp.some (try arg))
        <|> atom
    fun =
      try (TyArrow <$> funDom <*> (token TokArrow *> fun))
        <|> app
    funDom =
      try (paren ((,) <$> (Just <$> (lower <* token TokColon)) <*> fun))
        <|> ((Nothing,) <$> app)
    arg =
      try (PersistentArg <$> (token TokPersistent *> exprAtom))
        <|> (NormalArg <$> exprAtom)

parse :: P a -> Text -> Either String a
parse p source = do
  tokens <- Token.lex source
  mapLeft Mp.errorBundlePretty $ Mp.parse p "input" tokens

parseExpr :: Text -> Either String Expr
parseExpr = parse expr

parseTypeExpr :: Text -> Either String TypeExpr
parseTypeExpr = parse typeExpr
