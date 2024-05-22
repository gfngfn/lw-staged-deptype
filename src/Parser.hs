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

-- upper :: P Text
-- upper =
--   Mp.token
--     ( \case
--         TokUpper x -> Just x
--         _ -> Nothing
--     )
--     Set.empty

exprAtom, expr :: P Expr
(exprAtom, expr) = (atom, lam)
  where
    atom =
      try (Var <$> lower)
        <|> paren expr
    app =
      foldl1 App <$> Mp.some (try atom)
    lam =
      try (Lam <$> (token TokFun *> paren ((,) <$> lower <*> (token TokColon *> typeExpr)) <* token TokArrow) <*> expr)
        <|> app

typeExpr :: P TypeExpr
typeExpr = fun
  where
    atom =
      try (flip TyName [] <$> lower)
        <|> paren fun
    app =
      try (TyName <$> lower <*> Mp.some (try exprAtom))
        <|> atom
    fun =
      try (TyArrow <$> paren ((,) <$> (lower <* token TokColon) <*> fun) <*> (token TokArrow *> fun))
        <|> app

parse :: P a -> Text -> Either String a
parse p source = do
  tokens <- Token.lex source
  mapLeft Mp.errorBundlePretty $ Mp.parse p "input" tokens

parseExpr :: Text -> Either String Expr
parseExpr = parse expr

parseTypeExpr :: Text -> Either String TypeExpr
parseTypeExpr = parse typeExpr
