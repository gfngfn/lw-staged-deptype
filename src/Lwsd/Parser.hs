module Lwsd.Parser
  ( parseExpr,
    parseTypeExpr,
  )
where

import Control.Lens
import Data.Either.Extra
import Data.Functor
import Data.Generics.Labels ()
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Lwsd.Syntax
import Lwsd.Token (Token (..))
import Lwsd.Token qualified as Token
import Text.Megaparsec ((<|>))
import Text.Megaparsec qualified as Mp
import Util.ParserUtil
import Util.TokenUtil
import Prelude

type P a = GenP Token a

paren :: P a -> P a
paren p = token TokLeftParen *> p <* token TokRightParen

lower :: P (Located Text)
lower = expectToken (^? #_TokLower)

upper :: P (Located Text)
upper = expectToken (^? #_TokUpper)

int :: P (Located Int)
int = expectToken (^? #_TokInt)

vec :: P (Located [Int])
vec = genVec TokVecLeft TokVecRight TokSemicolon (noLoc int)

mat :: P (Located [[Int]])
mat = genMat TokMatLeft TokMatRight TokSemicolon TokColon (noLoc int)

makeBinary :: Expr -> (Located Var, Expr) -> Expr
makeBinary e1@(Expr loc1 _) (Located locBinOp binOp, e2@(Expr loc2 _)) =
  Expr (mergeSpan locLeft loc2) (App (Expr locLeft (App eOp e1)) e2)
  where
    locLeft = mergeSpan loc1 locBinOp
    eOp = Expr locBinOp (Var binOp)

exprAtom, expr :: P Expr
(exprAtom, expr) = (atom, letin)
  where
    atom :: P Expr
    atom =
      Mp.try (located (Literal . LitInt) <$> int)
        <|> Mp.try (located (Literal . LitVec) <$> vec)
        <|> Mp.try (located (Literal . LitMat) <$> mat)
        <|> Mp.try (located Var <$> lower)
        <|> (makeEnclosed <$> token TokLeftParen <*> expr <*> token TokRightParen)
      where
        located constructor (Located loc e) = Expr loc (constructor e)
        makeEnclosed loc1 (Expr _ e) loc2 = Expr (mergeSpan loc1 loc2) e

    staged :: P Expr
    staged =
      Mp.try (makeStaged Bracket <$> token TokBracket <*> staged)
        <|> Mp.try (makeStaged Escape <$> token TokEscape <*> staged)
        <|> atom
      where
        makeStaged constructor loc1 e@(Expr loc2 _) =
          Expr (mergeSpan loc1 loc2) (constructor e)

    app :: P Expr
    app =
      foldl1 makeApp <$> some (Mp.try staged)
      where
        makeApp :: Expr -> Expr -> Expr
        makeApp e1@(Expr loc1 _) e2@(Expr loc2 _) =
          Expr (mergeSpan loc1 loc2) (App e1 e2)

    mult :: P Expr
    mult =
      List.foldl' makeBinary <$> app <*> Mp.many (Mp.try ((,) <$> multOp <*> app))

    multOp :: P (Located Var)
    multOp =
      (\loc -> Located loc "*") <$> token TokOpMult

    add :: P Expr
    add =
      List.foldl' makeBinary <$> mult <*> Mp.many (Mp.try ((,) <$> addOp <*> mult))

    addOp :: P (Located Var)
    addOp =
      Mp.try ((\loc -> Located loc "+") <$> token TokOpAdd)
        <|> ((\loc -> Located loc "-") <$> token TokOpSub)

    lam :: P Expr
    lam =
      Mp.try (makeLam <$> token TokFun <*> (binder <* token TokArrow) <*> expr)
        <|> add
      where
        binder =
          paren ((,) <$> noLoc lower <*> (token TokColon *> typeExpr))

        makeLam locFirst (x, tye) e@(Expr locLast _) =
          Expr (mergeSpan locFirst locLast) (Lam (x, tye) e)

    letin :: P Expr
    letin =
      Mp.try (makeLetIn <$> token TokLet <*> noLoc lower <*> (token TokEqual *> letin) <*> (token TokIn *> letin))
        <|> lam
      where
        makeLetIn locFirst x e1 e2@(Expr locLast _) =
          Expr (mergeSpan locFirst locLast) (LetIn x e1 e2)

typeExpr :: P TypeExpr
typeExpr = fun
  where
    atom :: P TypeExpr
    atom =
      Mp.try ((\(Located loc t) -> TypeExpr loc (TyName t [])) <$> upper)
        <|> (makeEnclosed <$> token TokLeftParen <*> fun <*> token TokRightParen)
      where
        makeEnclosed loc1 (TypeExpr _ tyeMain) loc2 =
          TypeExpr (mergeSpan loc1 loc2) tyeMain

    staged :: P TypeExpr
    staged =
      Mp.try (makeTyCode <$> token TokBracket <*> staged)
        <|> atom
      where
        makeTyCode loc1 tye@(TypeExpr loc2 _) =
          TypeExpr (mergeSpan loc1 loc2) (TyCode tye)

    app :: P TypeExpr
    app =
      Mp.try (makeTyName <$> upper <*> some (Mp.try arg))
        <|> staged
      where
        makeTyName (Located locFirst t) tyeArgs =
          let loc =
                case NonEmpty.last tyeArgs of
                  PersistentArg (Expr locLast _) -> mergeSpan locFirst locLast
                  NormalArg (Expr locLast _) -> mergeSpan locFirst locLast
           in TypeExpr loc (TyName t (NonEmpty.toList tyeArgs))

    fun :: P TypeExpr
    fun =
      Mp.try (makeTyArrow <$> funDom <*> (token TokArrow *> fun))
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
      Mp.try (makeFunDom <$> token TokLeftParen <*> (noLoc lower <* token TokColon) <*> (fun <* token TokRightParen))
        <|> ((Nothing,) <$> app)
      where
        makeFunDom locFirst x tyeDom =
          (Just (locFirst, x), tyeDom)

    arg :: P ArgForType
    arg =
      Mp.try (PersistentArg <$> (token TokPersistent *> exprAtom))
        <|> (NormalArg <$> exprAtom)

parse :: P a -> Text -> Either String a
parse p source = do
  tokensWithOffsets <- Token.lex source
  mapLeft Mp.errorBundlePretty $ Mp.parse p "input" tokensWithOffsets

parseExpr :: Text -> Either String Expr
parseExpr = parse expr

parseTypeExpr :: Text -> Either String TypeExpr
parseTypeExpr = parse typeExpr
