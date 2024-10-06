module Surface.Parser
  ( parseExpr,
    parseTypeExpr,
  )
where

import Control.Lens
import Data.Either.Extra
import Data.Functor
import Data.Generics.Labels ()
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Surface.Syntax
import Surface.Token (Token (..))
import Surface.Token qualified as Token
import Util.ParserUtil
import Util.TokenUtil
import Prelude hiding (or)

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

makeBinOpApp :: Expr -> Located Var -> Expr -> Expr
makeBinOpApp e1@(Expr loc1 _) (Located locBinOp binOp) e2@(Expr loc2 _) =
  Expr (mergeSpan locLeft loc2) (App (Expr locLeft (App eOp e1)) e2)
  where
    locLeft = mergeSpan loc1 locBinOp
    eOp = Expr locBinOp (Var binOp)

exprAtom, expr :: P Expr
(exprAtom, expr) = (atom, letin)
  where
    atom :: P Expr
    atom =
      tries
        [ located (Literal . LitInt) <$> int,
          located (Literal . LitVec) <$> vec,
          located (Literal . LitMat) <$> mat,
          located Var <$> lower
        ]
        (makeEnclosed <$> token TokLeftParen <*> expr <*> token TokRightParen)
      where
        located constructor (Located loc e) = Expr loc (constructor e)
        makeEnclosed loc1 (Expr _ e) loc2 = Expr (mergeSpan loc1 loc2) e

    app :: P Expr
    app =
      foldl1 makeApp <$> some atom
      where
        makeApp :: Expr -> Expr -> Expr
        makeApp e1@(Expr loc1 _) e2@(Expr loc2 _) =
          Expr (mergeSpan loc1 loc2) (App e1 e2)

    mult :: P Expr
    mult =
      binSep makeBinOpApp multOp app
      where
        multOp :: P (Located Var)
        multOp =
          expectToken
            ( \case
                TokOpMult -> Just "*"
                _ -> Nothing
            )

    add :: P Expr
    add =
      binSep makeBinOpApp addOp mult
      where
        addOp :: P (Located Var)
        addOp =
          expectToken
            ( \case
                TokOpAdd -> Just "+"
                TokOpSub -> Just "-"
                _ -> Nothing
            )

    lam :: P Expr
    lam =
      (makeLam <$> token TokFun <*> (binder <* token TokArrow) <*> expr)
        `or` add
      where
        binder =
          paren ((,) <$> noLoc lower <*> (token TokColon *> typeExpr))

        makeLam locFirst (x, tye) e@(Expr locLast _) =
          Expr (mergeSpan locFirst locLast) (Lam (x, tye) e)

    letin :: P Expr
    letin =
      (makeLetIn <$> token TokLet <*> noLoc lower <*> (token TokEqual *> letin) <*> (token TokIn *> letin))
        `or` lam
      where
        makeLetIn locFirst x e1 e2@(Expr locLast _) =
          Expr (mergeSpan locFirst locLast) (LetIn x e1 e2)

typeExpr :: P TypeExpr
typeExpr = fun
  where
    atom :: P TypeExpr
    atom =
      ((\(Located loc t) -> TypeExpr loc (TyName t [])) <$> upper)
        `or` (makeEnclosed <$> token TokLeftParen <*> fun <*> token TokRightParen)
      where
        makeEnclosed loc1 (TypeExpr _ tyeMain) loc2 =
          TypeExpr (mergeSpan loc1 loc2) tyeMain

    app :: P TypeExpr
    app =
      (makeTyName <$> upper <*> some exprAtom)
        `or` atom
      where
        makeTyName (Located locFirst t) tyeArgs =
          let Expr locLast _ = NonEmpty.last tyeArgs
              loc = mergeSpan locFirst locLast
           in TypeExpr loc (TyName t (NonEmpty.toList tyeArgs))

    fun :: P TypeExpr
    fun =
      (makeTyArrow <$> funDom <*> (token TokArrow *> fun))
        `or` app
      where
        makeTyArrow funDomSpec tye2@(TypeExpr loc2 _) =
          let (loc, tyDom) =
                case funDomSpec of
                  (Nothing, tye1@(TypeExpr loc1 _)) -> (mergeSpan loc1 loc2, (Nothing, tye1))
                  (Just (loc1, x), tye1) -> (mergeSpan loc1 loc2, (Just x, tye1))
           in TypeExpr loc (TyArrow tyDom tye2)

    funDom :: P (Maybe (Span, Var), TypeExpr)
    funDom =
      (makeFunDom <$> token TokLeftParen <*> (noLoc lower <* token TokColon) <*> (fun <* token TokRightParen))
        `or` ((Nothing,) <$> app)
      where
        makeFunDom locFirst x tyeDom =
          (Just (locFirst, x), tyeDom)

parse :: P a -> Text -> Either String a
parse p source = do
  locatedTokens <- Token.lex source
  runParser p locatedTokens

parseExpr :: Text -> Either String Expr
parseExpr = parse expr

parseTypeExpr :: Text -> Either String TypeExpr
parseTypeExpr = parse typeExpr
