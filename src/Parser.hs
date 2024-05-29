module Parser
  ( parseExpr,
    parseTypeExpr,
  )
where

import Control.Comonad.Cofree
import Data.Either.Extra
import Data.Functor
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Void (Void)
import Syntax
import Text.Megaparsec hiding (Token, parse, token, tokens, some)
import Text.Megaparsec qualified as Mp
import Token (Token (..), Span, mergeSpan, Located (..))
import Token qualified
import Vector qualified
import Prelude

type P a = Mp.Parsec Void [Located Token] a

token :: Token -> P Span
token tExpected =
  Mp.token
    ( \(Located loc t) ->
        if t == tExpected then Just loc else Nothing
    )
    Set.empty

(=*>) :: forall f. P Span -> P (Cofree f Span) -> P (Located (Cofree f Span))
(=*>) pPrefix p = do
   (\loc1 e@(loc2 :< _) -> Located (mergeSpan loc1 loc2) e) <$> pPrefix <*> p
infixl 4 =*>

located :: (a -> f (Cofree f Span)) -> Located a -> Cofree f Span
located g (Located loc e) = loc :< g e

forgetLoc :: P (Located a) -> P a
forgetLoc p = (\(Located _ x) -> x) <$> p

-- (<*) :: P a -> P () -> P a
-- (<*) p pSuffix = do
--   (\(Located loc1 v) (Located loc2 ()) -> Located (mergeSpan loc1 loc2) v) <$> p Prelude.<*> pSuffix
-- infixl 4 <*
--
-- manyWithSepAndEnd :: P a -> P () -> P () -> P [a]
-- manyWithSepAndEnd p pSep pEnd =
--   try ((reform <$> p <*> Mp.many (pSep *> p)) <* pEnd)
--     <|> (fmap (const []) <$> pEnd)
--   where
--     reform (Located locFirst vFirst) rest =
--       case reverse rest of
--         [] ->
--           Located locFirst [vFirst]
--         Located locLast _ : _ ->
--           Located (mergeSpan locFirst locLast) (vFirst : map (\(Located _ v) -> v) rest)
--
-- some :: P a -> P (NonEmpty a)
-- some p = reform <$> Mp.some p
--   where
--     reform [] = error "Mp.some returns the empty list"
--     reform (Located locFirst vFirst : rest) =
--       Located locMerged (vFirst :| map (\(Located _ v) -> v) rest)
--       where
--         locMerged =
--           case reverse rest of
--             [] -> locFirst
--             Located locLast _ : _ -> mergeSpan locFirst locLast

paren :: P a -> P a
paren p = token TokLeftParen *> p <* token TokRightParen

lower :: P (Located Text)
lower =
  Mp.token
    ( \case
        Located loc (TokLower x) -> Just $ Located loc x
        _ -> Nothing
    )
    Set.empty

upper :: P (Located Text)
upper =
  Mp.token
    ( \case
        Located loc (TokUpper x) -> Just $ Located loc x
        _ -> Nothing
    )
    Set.empty

int :: P (Located Int)
int =
  Mp.token
    ( \case
        Located loc (TokInt n) -> Just $ Located loc n
        _ -> Nothing
    )
    Set.empty

vec :: P (Located [Int])
vec = makeVec <$> token TokVecLeft <*> rest
  where
    rest =
      try (makeNonemptyVec <$> forgetLoc int <*> Mp.many (token TokSemicolon *> forgetLoc int) <*> token TokVecRight)
        <|> (([],) <$> token TokVecRight)

    makeNonemptyVec elemFirst elemsTail locLast =
      (elemFirst : elemsTail, locLast)

    makeVec locFirst (elems, locLast) =
      Located (mergeSpan locFirst locLast) elems

exprAtom, expr :: P Expr
(exprAtom, expr) = (atom, letin)
  where
    atom :: P Expr
    atom =
      try (located (Literal . LitInt) <$> int)
        <|> try (located (Literal . LitVec . Vector.fromList) <$> vec)
        <|> try (located Var <$> lower)
        <|> paren expr

    staged :: P Expr
    staged =
      try (located Bracket <$> (token TokBracket =*> staged))
        <|> try (located Escape <$> (token TokEscape =*> staged))
        <|> atom

    app :: P Expr
    app =
      foldl1 makeApp <$> Mp.some (try staged)
      where
        makeApp e1@(loc1 :< _) e2@(loc2 :< _) =
          mergeSpan loc1 loc2 :< App e1 e2

    lam :: P Expr
    lam =
      try (makeLam <$> token TokFun <*> (paren ((,) <$> forgetLoc lower <*> (token TokColon *> typeExpr)) <* token TokArrow) <*> expr)
        <|> app
      where
        makeLam locFirst binder e@(locLast :< _) =
          mergeSpan locFirst locLast :< Lam binder e

    letin :: P Expr
    letin =
      try (makeLetIn <$> token TokLet <*> forgetLoc lower <*> (token TokEqual *> letin) <*> (token TokIn *> letin))
        <|> lam
      where
        makeLetIn locFirst x e1 e2@(locLast :< _) =
          mergeSpan locFirst locLast :< LetIn x e1 e2

typeExpr :: P TypeExpr
typeExpr = fun
  where
    atom :: P TypeExpr
    atom =
      try (located (flip TyName []) <$> upper)
        <|> paren fun

    staged :: P TypeExpr
    staged =
      try (located TyCode <$> (token TokBracket =*> staged))
        <|> atom

    app :: P TypeExpr
    app =
      try (makeTyName <$> upper <*> Mp.some (try arg))
        <|> staged
      where
        makeTyName (Located locFirst t) tyeArgs =
          let loc =
                case reverse tyeArgs of
                  [] -> error "Mp.some returned the empty list"
                  PersistentArg (locLast :< _) : _ -> mergeSpan locFirst locLast
                  NormalArg (locLast :< _) : _ -> mergeSpan locFirst locLast
           in loc :< TyName t tyeArgs

    fun :: P TypeExpr
    fun =
      try (makeTyArrow <$> funDom <*> (token TokArrow *> fun))
        <|> app
      where
        makeTyArrow funDomSpec tye2@(loc2 :< _) =
          let (loc, tyDom) =
                case funDomSpec of
                  (Nothing, tye1@(loc1 :< _)) -> (mergeSpan loc1 loc2, (Nothing, tye1))
                  (Just (loc1, x), tye1) -> (mergeSpan loc1 loc2, (Just x, tye1))
           in loc :< TyArrow tyDom tye2

    funDom :: P (Maybe (Span, Var), TypeExpr)
    funDom =
      try (makeFunDom <$> token TokLeftParen <*> (forgetLoc lower <* token TokColon) <*> fun <* token TokRightParen)
        <|> ((Nothing,) <$> app)
      where
        makeFunDom locFirst x tyeDom =
          (Just (locFirst, x), tyeDom)

    arg :: P ArgForType
    arg =
      try (PersistentArg <$> (token TokPersistent *> exprAtom))
        <|> (NormalArg <$> exprAtom)

parse :: P a -> Text -> Either String a
parse p source = do
  tokensWithOffsets <- Token.lex source
  mapLeft Mp.errorBundlePretty $ Mp.parse p "input" tokensWithOffsets

parseExpr :: Text -> Either String Expr
parseExpr = parse expr

parseTypeExpr :: Text -> Either String TypeExpr
parseTypeExpr = parse typeExpr
