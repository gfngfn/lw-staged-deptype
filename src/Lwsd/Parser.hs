module Lwsd.Parser
  ( parseExpr,
    parseTypeExpr, -- Made public for tests
    parseDecls,
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
import Util.TokenUtil
import Prelude hiding (or)

type P a = GenP Token a

data BinderKind
  = MandatoryBinder (Var, TypeExpr)
  | OptionalBinder (Var, TypeExpr)

paren :: P a -> P a
paren p = token TokLeftParen *> p <* token TokRightParen

brace :: P a -> P a
brace p = token TokLeftBrace *> p <* token TokRightBrace

lower :: P (Located Text)
lower = expectToken (^? #_TokLower)

upper :: P (Located Text)
upper = expectToken (^? #_TokUpper)

longOrShortLower :: P (Located ([Text], Text))
longOrShortLower =
  expectToken (^? #_TokLongLower)
    `or` (fmap ([],) <$> lower)

standaloneOp :: P (Located Text)
standaloneOp = makeOp <$> token TokLeftParen <*> operator <*> token TokRightParen
  where
    makeOp locLeft (Located _ opName) locRight = Located (mergeSpan locLeft locRight) opName

boundIdent :: P (Located Text)
boundIdent = lower `or` standaloneOp

int :: P (Located Int)
int = expectToken (^? #_TokInt)

float :: P (Located Double)
float = expectToken (^? #_TokFloat)

string :: P (Located Text)
string = expectToken (^? #_TokString)

vec :: P (Located [Int])
vec = genVec TokVecLeft TokVecRight TokSemicolon (noLoc int)

list :: P a -> P (Located [a])
list = genVec TokLeftSquare TokRightSquare TokComma

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
  | FunArgOptGiven Span Expr Span
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
          located (\x -> Var ([], x)) <$> standaloneOp
        ]
        (makeEnclosed <$> token TokLeftParen <*> expr <*> token TokRightParen)
      where
        located constructor (Located loc e) = Expr loc (constructor e)
        makeEnclosed loc1 (Expr _ e) loc2 = Expr (mergeSpan loc1 loc2) e

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
            [ FunArgMandatory <$> staged,
              FunArgOptGiven <$> token TokLeftBrace <*> letin <*> token TokRightBrace
            ]
            (FunArgOptOmitted <$> token TokUnderscore)

        makeApp :: NonEmpty FunArg -> P Expr
        makeApp (FunArgMandatory eFun :| args) = pure $ List.foldl' makeAppSingle eFun args
        makeApp (FunArgOptGiven loc _ _ :| _) = failure (Located loc TokLeftBrace)
        makeApp (FunArgOptOmitted loc :| _) = failure (Located loc TokUnderscore)

        makeAppSingle :: Expr -> FunArg -> Expr
        makeAppSingle e1@(Expr loc1 _) = \case
          FunArgMandatory e2@(Expr loc2 _) -> Expr (mergeSpan loc1 loc2) (App e1 e2)
          FunArgOptGiven _ e2 loc2 -> Expr (mergeSpan loc1 loc2) (AppOptGiven e1 e2)
          FunArgOptOmitted loc2 -> Expr (mergeSpan loc1 loc2) (AppOptOmitted e1)

    as :: P Expr
    as =
      (makeAs <$> app <*> (token TokAs *> typeExpr))
        `or` app
      where
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
          paren ((,) <$> noLoc lower <*> (token TokColon *> typeExpr))

        optionalBinder =
          brace ((,) <$> noLoc lower <*> (token TokColon *> typeExpr))

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
      (makeLetIn <$> token TokLet <*> noLoc boundIdent <*> (token TokEqual *> letin) <*> (token TokIn *> letin))
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
        [ makeFunDom True <$> token TokLeftParen <*> (noLoc lower <* token TokColon) <*> (fun <* token TokRightParen),
          makeFunDom False <$> token TokLeftBrace <*> (noLoc lower <* token TokColon) <*> (fun <* token TokRightBrace)
        ]
        ((Nothing,) <$> app)
      where
        makeFunDom isMandatory locFirst x tyeDom =
          (Just (isMandatory, locFirst, x), tyeDom)

decl :: P Decl
decl =
  (makeDeclVal <$> token TokVal <*> valBinder <*> (token TokColon *> typeExpr) <*> (token TokExternal *> external) <*> string)
    `or` (makeDeclModule <$> token TokModule <*> noLoc upper <*> (token TokColon *> token TokSig *> many decl) <*> token TokEnd)
  where
    valBinder :: P (TypeExpr -> External -> Text -> DeclMainF Span)
    valBinder =
      tries
        [ DeclVal0 <$> (token TokEscape *> noLoc boundIdent),
          DeclValPers <$> (token TokPersistent *> noLoc boundIdent)
        ]
        (DeclVal1 <$> noLoc boundIdent)

    makeDeclVal locFirst ctor tye ext (Located locLast surf) =
      Decl (mergeSpan locFirst locLast) (ctor tye ext surf)

    makeDeclModule locFirst m decls locLast =
      Decl (mergeSpan locFirst locLast) (DeclModule m decls)

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

parseDecls :: Text -> Either String [Decl]
parseDecls = parse (manyNoTry decl <* eof)
