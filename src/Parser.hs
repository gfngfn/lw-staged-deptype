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

forgetLoc :: P (Located a) -> P a
forgetLoc p = (\(Located _ x) -> x) <$> p

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
        <|> makeEnclosed <$> token TokLeftParen <*> expr <*> token TokRightParen
      where
        located constructor (Located loc e) = Expr loc (constructor e)
        makeEnclosed loc1 (Expr _ e) loc2 = Expr (mergeSpan loc1 loc2) e

    staged :: P Expr
    staged =
      try (makeStaged Bracket <$> token TokBracket <*> staged)
        <|> try (makeStaged Escape <$> token TokEscape <*> staged)
        <|> atom
      where
        makeStaged constructor loc1 e@(Expr loc2 _) =
          Expr (mergeSpan loc1 loc2) (constructor e)

    app :: P Expr
    app =
      foldl1 makeApp <$> Mp.some (try staged)
      where
        makeApp :: Expr -> Expr -> Expr
        makeApp e1@(Expr loc1 _) e2@(Expr loc2 _) =
          Expr (mergeSpan loc1 loc2) (App e1 e2)

    lam :: P Expr
    lam =
      try (makeLam <$> token TokFun <*> (paren ((,) <$> forgetLoc lower <*> (token TokColon *> typeExpr)) <* token TokArrow) <*> expr)
        <|> app
      where
        makeLam locFirst (x, tye) e@(Expr locLast _) =
          Expr (mergeSpan locFirst locLast) (Lam (x, tye) e)

    letin :: P Expr
    letin =
      try (makeLetIn <$> token TokLet <*> forgetLoc lower <*> (token TokEqual *> letin) <*> (token TokIn *> letin))
        <|> lam
      where
        makeLetIn locFirst x e1 e2@(Expr locLast _) =
          Expr (mergeSpan locFirst locLast) (LetIn x e1 e2)

typeExpr :: P TypeExpr
typeExpr = fun
  where
    atom :: P TypeExpr
    atom =
      try ((\(Located loc t) -> TypeExpr loc (TyName t [])) <$> upper)
        <|> makeEnclosed <$> token TokLeftParen <*> fun <*> token TokRightParen
      where
        makeEnclosed loc1 (TypeExpr _ tyeMain) loc2 =
          TypeExpr (mergeSpan loc1 loc2) tyeMain

    staged :: P TypeExpr
    staged =
      try (makeTyCode <$> token TokBracket <*> staged)
        <|> atom
      where
        makeTyCode loc1 tye@(TypeExpr loc2 _) =
          TypeExpr (mergeSpan loc1 loc2) (TyCode tye)

    app :: P TypeExpr
    app =
      try (makeTyName <$> upper <*> Mp.some (try arg))
        <|> staged
      where
        makeTyName (Located locFirst t) tyeArgs =
          let loc =
                case reverse tyeArgs of
                  [] -> error "Mp.some returned the empty list"
                  PersistentArg (Expr locLast _) : _ -> mergeSpan locFirst locLast
                  NormalArg (Expr locLast _) : _ -> mergeSpan locFirst locLast
           in TypeExpr loc (TyName t tyeArgs)

    fun :: P TypeExpr
    fun =
      try (makeTyArrow <$> funDom <*> (token TokArrow *> fun))
        <|> app
      where
        makeTyArrow funDomSpec tye2@(TypeExpr loc2 _) =
          let (loc, tyDom) =
                case funDomSpec of
                  (Nothing, tye1@(TypeExpr loc1 _)) -> (mergeSpan loc1 loc2, (Nothing, tye1))
                  (Just (loc1, x), tye1) -> (mergeSpan loc1 loc2, (Just x, tye1))
           in TypeExpr loc (TyArrow tyDom tye2)

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
