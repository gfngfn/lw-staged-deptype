module Parser
  ( parseExpr,
    parseTypeExpr,
  )
where

import Data.Either.Extra
import Data.Functor
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Void (Void)
import Syntax
import Text.Megaparsec hiding (Token, parse, token, tokens, some)
import Text.Megaparsec qualified as Mp
import Token (Token (..), mergeSpan, Located (..))
import Token qualified
import Vector qualified
import Prelude hiding (mod, (*>), (<*))

type P a = Mp.Parsec Void [Located Token] (Located a)

token :: Token -> P ()
token tExpected =
  Mp.token
    ( \(Located loc t) ->
        if t == tExpected then Just (Located loc ()) else Nothing
    )
    Set.empty

(*>) :: P () -> P a -> P a
(*>) pPrefix p = do
  (\(Located loc1 ()) (Located loc2 v) -> Located (mergeSpan loc1 loc2) v) <$> pPrefix Prelude.<*> p
infixl 4 *>

(<*) :: P a -> P () -> P a
(<*) p pSuffix = do
  (\(Located loc1 v) (Located loc2 ()) -> Located (mergeSpan loc1 loc2) v) <$> p Prelude.<*> pSuffix
infixl 4 <*

manyWithSepAndEnd :: P a -> P () -> P () -> P [a]
manyWithSepAndEnd p pSep pEnd =
  try ((reform <$> p <*> Mp.many (pSep *> p)) <* pEnd)
    <|> (fmap (const []) <$> pEnd)
  where
    reform (Located locFirst vFirst) rest =
      case reverse rest of
        [] ->
          Located locFirst [vFirst]
        Located locLast _ : _ ->
          Located (mergeSpan locFirst locLast) (vFirst : map (\(Located _ v) -> v) rest)

some :: P a -> P (NonEmpty a)
some p = reform <$> Mp.some p
  where
    reform [] = error "Mp.some returns the empty list"
    reform (Located locFirst vFirst : rest) =
      Located locMerged (vFirst :| map (\(Located _ v) -> v) rest)
      where
        locMerged =
          case reverse rest of
            [] -> locFirst
            Located locLast _ : _ -> mergeSpan locFirst locLast

paren :: P a -> P a
paren p = token TokLeftParen *> p <* token TokRightParen

lower :: P Text
lower =
  Mp.token
    ( \case
        Located loc (TokLower x) -> Just $ Located loc x
        _ -> Nothing
    )
    Set.empty

upper :: P Text
upper =
  Mp.token
    ( \case
        Located loc (TokUpper x) -> Just $ Located loc x
        _ -> Nothing
    )
    Set.empty

int :: P Int
int =
  Mp.token
    ( \case
        Located loc (TokInt n) -> Just $ Located loc n
        _ -> Nothing
    )
    Set.empty

vec :: P [Int]
vec = token TokVecLeft *> manyWithSepAndEnd int (token TokSemicolon) (token TokVecRight)

exprAtom, expr :: P Expr
(exprAtom, expr) = (atom, letin)
  where
    atom :: P Expr
    atom =
      try (fmap (Literal . LitInt) <$> int)
        <|> try (fmap (Literal . LitVec . Vector.fromList) <$> vec)
        <|> try (fmap Var <$> lower)
        <|> paren expr

    staged :: P Expr
    staged =
      try (fmap Bracket <$> (token TokBracket *> staged))
        <|> try (fmap Escape <$> (token TokEscape *> staged))
        <|> atom

    app :: P Expr
    app =
      fmap (foldl1 App) <$> some (try staged)

    lam :: P Expr
    lam =
      try (fmap2 Lam <$> (token TokFun *> paren (fmap2 (,) <$> lower <*> (token TokColon *> typeExpr)) <* token TokArrow) <*> expr)
        <|> app

    letin :: P Expr
    letin =
      try (fmap3 LetIn <$> (token TokLet *> lower) <*> (token TokEqual *> letin) <*> (token TokIn *> letin))
        <|> lam

typeExpr :: P TypeExpr
typeExpr = fun
  where
    atom :: P TypeExpr
    atom =
      try (fmap (flip TyName []) <$> upper)
        <|> paren fun

    staged :: P TypeExpr
    staged =
      try (fmap TyCode <$> (token TokBracket *> staged))
        <|> atom

    app :: P TypeExpr
    app =
      try (fmap2 TyName <$> upper <*> Mp.some (try arg))
        <|> staged

    fun :: P TypeExpr
    fun =
      try (fmap2 TyArrow <$> funDom <*> (token TokArrow *> fun))
        <|> app

    funDom :: P (Maybe Var, TypeExpr)
    funDom =
      try (paren (fmap2 (,) <$> (Just <$> (lower <* token TokColon)) <*> fun))
        <|> (fmap (Nothing,) <$> app)

    arg :: P ArgForType
    arg =
      try (fmap PersistentArg <$> (token TokPersistent *> exprAtom))
        <|> (fmap NormalArg <$> exprAtom)

parse :: P a -> Text -> Either String (Located a)
parse p source = do
  tokensWithOffsets <- Token.lex source
  mapLeft Mp.errorBundlePretty $ Mp.parse p "input" tokensWithOffsets

parseExpr :: Text -> Either String Expr
parseExpr = parse expr

parseTypeExpr :: Text -> Either String TypeExpr
parseTypeExpr = parse typeExpr
