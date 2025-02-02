module Lwsd.Parser
  ( parseExpr,
    parseTypeExpr, -- Made public for tests
    parseBinds,
  )
where

import Control.Lens
import Data.Either.Extra
import Data.Functor
import Data.Generics.Labels ()
import Data.List.Extra qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Lwsd.SrcSyntax
import Lwsd.Token (Token (..))
import Lwsd.Token qualified as Token
import Util.ParserUtil
import Util.TokenUtil (Located (..), Span, mergeSpan)
import Prelude hiding (or)

type P a = GenP Token a

data BinderKind
  = MandatoryBinder (Var, TypeExpr)
  | OptionalBinder (Var, TypeExpr)

parenGen :: Token -> Token -> P a -> P (Located a)
parenGen tokLeft tokRight p =
  make <$> token tokLeft <*> p <*> token tokRight
  where
    make locLeft v locRight = Located (mergeSpan locLeft locRight) v

paren :: P a -> P (Located a)
paren = parenGen TokLeftParen TokRightParen

brace :: P a -> P (Located a)
brace = parenGen TokLeftBrace TokRightBrace

lower :: P (Located Text)
lower = expectToken (^? #_TokLower)

upper :: P (Located Text)
upper = expectToken (^? #_TokUpper)

longOrShortLower :: P (Located ([Text], Text))
longOrShortLower =
  expectToken (^? #_TokLongLower)
    `or` (fmap ([],) <$> lower)

standaloneOp :: P (Located Text)
standaloneOp = paren (noLoc operator)

boundIdent :: P (Located Text)
boundIdent = lower `or` standaloneOp

int :: P (Located Int)
int = expectToken (^? #_TokInt)

float :: P (Located Double)
float = expectToken (^? #_TokFloat)

string :: P (Located Text)
string = expectToken (^? #_TokString)

list :: P a -> P (Located [a])
list = genVec TokLeftSquare TokRightSquare TokComma

vec :: P (Located [Int])
vec = genVec TokVecLeft TokVecRight TokSemicolon (noLoc int)

mat :: P (Located [[Int]])
mat = genMat TokMatLeft TokMatRight TokSemicolon TokComma (noLoc int)

operator :: P (Located Var)
operator = tries [compOp, addOp] multOp

multOp :: P (Located Var)
multOp = expectToken (^? #_TokOpMult)

addOp :: P (Located Var)
addOp = expectToken (^? #_TokOpAdd)

compOp :: P (Located Var)
compOp = expectToken (^? #_TokOpComp)

makeBinOpApp :: Expr -> Located Var -> Expr -> Expr
makeBinOpApp e1@(Expr loc1 _) (Located locBinOp binOp) e2@(Expr loc2 _) =
  Expr (mergeSpan locLeft loc2) (App (Expr locLeft (App eOp e1)) e2)
  where
    locLeft = mergeSpan loc1 locBinOp
    eOp = Expr locBinOp (Var ([], binOp))

data FunArg
  = FunArgMandatory Expr
  | FunArgOptGiven (Located Expr)
  | FunArgOptOmitted Span

exprAtom, expr :: P Expr
(exprAtom, expr) = (atom, letin)
  where
    atom :: P Expr
    atom =
      tries
        [ located (Literal . LitInt) <$> int,
          located (Literal . LitFloat) <$> float,
          located (Literal . LitList) <$> list letin,
          located (Literal . LitVec) <$> vec,
          located (Literal . LitMat) <$> mat,
          located Var <$> longOrShortLower,
          located (\x -> Var ([], x)) <$> standaloneOp,
          makeLitUnit <$> token TokLeftParen <*> token TokRightParen
        ]
        (makeEnclosed <$> paren expr)
      where
        located constructor (Located loc e) = Expr loc (constructor e)
        makeLitUnit loc1 loc2 = Expr (mergeSpan loc1 loc2) (Literal LitUnit)
        makeEnclosed (Located loc (Expr _ eMain)) = Expr loc eMain

    staged :: P Expr
    staged =
      tries
        [ makeStaged Bracket <$> token TokBracket <*> staged,
          makeStaged Escape <$> token TokEscape <*> staged
        ]
        atom
      where
        makeStaged constructor loc1 e@(Expr loc2 _) =
          Expr (mergeSpan loc1 loc2) (constructor e)

    app :: P Expr
    app =
      some arg >>= makeApp
      where
        arg :: P FunArg
        arg =
          tries
            [ FunArgOptOmitted <$> token TokUnderscore,
              FunArgOptGiven <$> brace letin
            ]
            (FunArgMandatory <$> staged)

        makeApp :: NonEmpty FunArg -> P Expr
        makeApp (FunArgMandatory eFun :| args) = pure $ List.foldl' makeAppSingle eFun args
        makeApp (FunArgOptGiven (Located loc _e) :| _) = failure (Located loc TokLeftBrace)
        makeApp (FunArgOptOmitted loc :| _) = failure (Located loc TokUnderscore)

        makeAppSingle :: Expr -> FunArg -> Expr
        makeAppSingle e1@(Expr loc1 _) = \case
          FunArgMandatory e2@(Expr loc2 _) -> Expr (mergeSpan loc1 loc2) (App e1 e2)
          FunArgOptGiven (Located loc2 e2) -> Expr (mergeSpan loc1 loc2) (AppOptGiven e1 e2)
          FunArgOptOmitted loc2 -> Expr (mergeSpan loc1 loc2) (AppOptOmitted e1)

    as :: P Expr
    as =
      (makeAs <$> app <*> (token TokAs *> typeExpr))
        `or` app
      where
        makeAs :: Expr -> TypeExpr -> Expr
        makeAs e1@(Expr loc1 _) tye2@(TypeExpr loc2 _) =
          Expr (mergeSpan loc1 loc2) (As e1 tye2)

    mult :: P Expr
    mult = binSep makeBinOpApp multOp as

    add :: P Expr
    add = binSep makeBinOpApp addOp mult

    comp :: P Expr
    comp = binSep makeBinOpApp compOp add

    lam :: P Expr
    lam =
      tries
        [ makeNonrecLam <$> token TokFun <*> (binder <* token TokArrow) <*> expr,
          makeRecLam <$> token TokRec <*> (mandatoryBinder <* token TokArrow <* token TokFun) <*> (mandatoryBinder <* token TokArrow) <*> expr,
          makeIf <$> token TokIf <*> expr <*> (token TokThen *> expr) <*> (token TokElse *> expr)
        ]
        comp
      where
        binder =
          (MandatoryBinder <$> mandatoryBinder) `or` (OptionalBinder <$> optionalBinder)

        mandatoryBinder =
          noLoc (paren ((,) <$> noLoc lower <*> (token TokColon *> typeExpr)))

        optionalBinder =
          noLoc (brace ((,) <$> noLoc lower <*> (token TokColon *> typeExpr)))

        makeNonrecLam locFirst xBinder' e@(Expr locLast _) =
          Expr (mergeSpan locFirst locLast) $
            case xBinder' of
              MandatoryBinder xBinder -> Lam Nothing xBinder e
              OptionalBinder xBinder -> LamOpt xBinder e

        makeRecLam locFirst fBinder xBinder e@(Expr locLast _) =
          Expr (mergeSpan locFirst locLast) (Lam (Just fBinder) xBinder e)

        makeIf locFirst e0 e1 e2@(Expr locLast _) =
          Expr (mergeSpan locFirst locLast) (IfThenElse e0 e1 e2)

    letin :: P Expr
    letin =
      tries
        [ makeLetIn <$> token TokLet <*> noLoc boundIdent <*> (token TokEqual *> letin) <*> (token TokIn *> letin),
          makeLetOpenIn <$> token TokLet <*> (token TokOpen *> noLoc upper) <*> (token TokIn *> letin),
          makeSequential <$> (comp <* token TokSemicolon) <*> letin
        ]
        lam
      where
        makeLetIn locFirst x e1 e2@(Expr locLast _) =
          Expr (mergeSpan locFirst locLast) (LetIn x e1 e2)

        makeLetOpenIn locFirst m e@(Expr locLast _) =
          Expr (mergeSpan locFirst locLast) (LetOpenIn m e)

        makeSequential e1@(Expr locFirst _) e2@(Expr locLast _) =
          Expr (mergeSpan locFirst locLast) (Sequential e1 e2)

typeExpr :: P TypeExpr
typeExpr = fun
  where
    atom :: P TypeExpr
    atom =
      tries
        [ makeNamed <$> upper,
          makeRefinement <$> paren ((,,) <$> (noLoc boundIdent <* token TokColon) <*> (fun <* token TokBar) <*> expr)
        ]
        (makeEnclosed <$> paren fun)
      where
        makeNamed (Located loc t) = TypeExpr loc (TyName t [])
        makeRefinement (Located loc (x, tye, e)) = TypeExpr loc (TyRefinement x tye e)
        makeEnclosed (Located loc (TypeExpr _ tyeMain)) = TypeExpr loc tyeMain

    staged :: P TypeExpr
    staged =
      (makeTyCode <$> token TokBracket <*> staged)
        `or` atom
      where
        makeTyCode loc1 tye@(TypeExpr loc2 _) =
          TypeExpr (mergeSpan loc1 loc2) (TyCode tye)

    app :: P TypeExpr
    app =
      (makeTyName <$> upper <*> some arg)
        `or` staged
      where
        arg :: P ArgForType
        arg =
          tries
            [ ExprArgPersistent <$> (token TokPersistent *> exprAtom),
              ExprArgNormal <$> exprAtom
            ]
            (TypeArg <$> atom)

        makeTyName (Located locFirst t) tyeArgs =
          let loc =
                case NonEmpty.last tyeArgs of
                  ExprArgPersistent (Expr locLast _) -> mergeSpan locFirst locLast
                  ExprArgNormal (Expr locLast _) -> mergeSpan locFirst locLast
                  TypeArg (TypeExpr locLast _) -> mergeSpan locFirst locLast
           in TypeExpr loc (TyName t (NonEmpty.toList tyeArgs))

    fun :: P TypeExpr
    fun =
      (makeTyArrow <$> funDom <*> (token TokArrow *> fun))
        `or` app
      where
        makeTyArrow funDomSpec tye2@(TypeExpr loc2 _) =
          case funDomSpec of
            (Nothing, tye1@(TypeExpr loc1 _)) ->
              TypeExpr (mergeSpan loc1 loc2) (TyArrow (Nothing, tye1) tye2)
            (Just (isMandatory, loc1, x), tye1) ->
              TypeExpr (mergeSpan loc1 loc2) $
                if isMandatory
                  then TyArrow (Just x, tye1) tye2
                  else TyOptArrow (x, tye1) tye2

    funDom :: P (Maybe (Bool, Span, Var), TypeExpr)
    funDom =
      tries
        [ makeFunDom True <$> paren ((,) <$> (noLoc lower <* token TokColon) <*> fun),
          makeFunDom False <$> brace ((,) <$> (noLoc lower <* token TokColon) <*> fun)
        ]
        ((Nothing,) <$> app)
      where
        makeFunDom isMandatory (Located loc (x, tyeDom)) = (Just (isMandatory, loc, x), tyeDom)

bind :: P Bind
bind =
  (makeBindVal <$> token TokVal <*> valBinder <*> bindVal)
    `or` (makeBindModule <$> token TokModule <*> noLoc upper <*> (token TokEqual *> token TokStruct *> many bind) <*> token TokEnd)
  where
    makeBindVal locFirst (stage, x) (bv, locLast) =
      Bind (mergeSpan locFirst locLast) (BindVal stage x bv)

    makeBindModule locFirst m binds locLast =
      Bind (mergeSpan locFirst locLast) (BindModule m binds)

valBinder :: P (Stage, Var)
valBinder =
  tries
    [ (Stage0,) <$> (token TokEscape *> noLoc boundIdent),
      (StagePers,) <$> (token TokPersistent *> noLoc boundIdent)
    ]
    ((Stage1,) <$> noLoc boundIdent)

bindVal :: P (BindVal, Span)
bindVal =
  (makeBindValExternal <$> (token TokColon *> typeExpr) <*> (token TokExternal *> external) <*> string)
    `or` ((\e@(Expr locLast _) -> (BindValNormal e, locLast)) <$> (token TokEqual *> expr))
  where
    makeBindValExternal ty ext (Located locLast surfaceName) =
      (BindValExternal ty ext surfaceName, locLast)

external :: P External
external = noLoc string

parse :: P a -> Text -> Either String a
parse p source = do
  locatedTokens <- Token.lex source
  runParser p locatedTokens

parseExpr :: Text -> Either String Expr
parseExpr = parse (expr <* eof)

parseTypeExpr :: Text -> Either String TypeExpr
parseTypeExpr = parse (typeExpr <* eof)

parseBinds :: Text -> Either String [Bind]
parseBinds = parse (manyNoTry bind <* eof)
