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
import Util.FrontError (FrontError (..))
import Util.LocationInFile (SourceSpec)
import Util.ParserUtil
import Util.TokenUtil (Located (..), Span, mergeSpan)
import Prelude hiding (or)

type P a = GenP Token a

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
    <|> (fmap ([],) <$> lower)

typeVar :: P (Located TypeVar)
typeVar = fmap TypeVar <$> expectToken (^? #_TokTypeVar)

standaloneOp :: P (Located Text)
standaloneOp = paren (noLoc operator)

boundIdent :: P (Located Text)
boundIdent = lower <|> standaloneOp

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
operator = compOp <|> addOp <|> multOp

multOp :: P (Located Var)
multOp =
  (fmap (const "*") <$> expectToken (^? #_TokProd))
    <|> expectToken (^? #_TokOpMult)

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
      (located (Literal . LitInt) <$> int)
        <|> (located (Literal . LitFloat) <$> float)
        <|> (located (Literal . LitString) <$> string)
        <|> (located (Literal . LitList) <$> list letin)
        <|> (located (Literal . LitVec) <$> vec)
        <|> (located (Literal . LitMat) <$> mat)
        <|> (makeBool True <$> token TokTrue)
        <|> (makeBool False <$> token TokFalse)
        <|> (located Var <$> longOrShortLower)
        <|> try (located (\x -> Var ([], x)) <$> standaloneOp)
        <|> try (makeLitUnit <$> token TokLeftParen <*> token TokRightParen)
        <|> try (makeTuple <$> paren ((,) <$> (expr <* token TokComma) <*> expr))
        <|> (makeEnclosed <$> paren expr)
      where
        located constructor (Located loc e) = Expr loc (constructor e)
        makeLitUnit loc1 loc2 = Expr (mergeSpan loc1 loc2) (Literal LitUnit)
        makeTuple (Located loc (e1, e2)) = Expr loc (Tuple e1 e2)
        makeEnclosed (Located loc (Expr _ eMain)) = Expr loc eMain
        makeBool b loc = Expr loc (Literal (LitBool b))

    staged :: P Expr
    staged =
      (makeStaged Bracket <$> token TokBracket <*> staged)
        <|> (makeStaged Escape <$> token TokEscape <*> staged)
        <|> atom
      where
        makeStaged constructor loc1 e@(Expr loc2 _) =
          Expr (mergeSpan loc1 loc2) (constructor e)

    app :: P Expr
    app =
      some arg >>= makeApp
      where
        arg :: P FunArg
        arg =
          (FunArgOptOmitted <$> token TokUnderscore)
            <|> (FunArgOptGiven <$> brace letin)
            <|> (FunArgMandatory <$> staged)

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
      makeAs <$> app <*> optional (token TokAs *> typeExpr)
      where
        makeAs :: Expr -> Maybe TypeExpr -> Expr
        makeAs e1@(Expr loc1 _) = \case
          Nothing -> e1
          Just tye2@(TypeExpr loc2 _) -> Expr (mergeSpan loc1 loc2) (As e1 tye2)

    mult :: P Expr
    mult = binSep makeBinOpApp multOp as

    add :: P Expr
    add = binSep makeBinOpApp addOp mult

    comp :: P Expr
    comp = binSep makeBinOpApp compOp add

    flipApp :: P Expr
    flipApp = makeFlipApp <$> comp <*> many (token TokOpFlipApp *> comp)
      where
        makeFlipApp =
          List.foldl'
            ( \eArg@(Expr locArg _) eFun@(Expr locFun _) ->
                Expr (mergeSpan locArg locFun) (App eFun eArg)
            )

    lam :: P Expr
    lam =
      (makeNonrecLam <$> token TokFun <*> (lamBinder <* token TokArrow) <*> expr)
        <|> (makeRecLam <$> token TokRec <*> (mandatoryBinder <* token TokArrow <* token TokFun) <*> (mandatoryBinder <* token TokArrow) <*> expr)
        <|> (makeIf <$> token TokIf <*> expr <*> (token TokThen *> expr) <*> (token TokElse *> expr))
        <|> flipApp
      where
        makeNonrecLam locFirst xBinder' e@(Expr locLast _) =
          Expr (mergeSpan locFirst locLast) $
            case xBinder' of
              MandatoryBinder xBinder -> Lam Nothing xBinder e
              OptionalBinder xBinder -> LamOpt xBinder e

        makeRecLam locFirst fBinder xBinder e@(Expr locLast _) =
          Expr (mergeSpan locFirst locLast) (Lam (Just fBinder) xBinder e)

        makeIf locFirst e0 e1 e2@(Expr locLast _) =
          Expr (mergeSpan locFirst locLast) (IfThenElse e0 e1 e2)

    lamBinder :: P LamBinder
    lamBinder =
      (MandatoryBinder <$> mandatoryBinder) <|> (OptionalBinder <$> optionalBinder)

    mandatoryBinder, optionalBinder :: P (Var, TypeExpr)
    mandatoryBinder = noLoc (paren ((,) <$> noLoc lower <*> (token TokColon *> typeExpr)))
    optionalBinder = noLoc (brace ((,) <$> noLoc lower <*> (token TokColon *> typeExpr)))

    letin :: P Expr
    letin =
      (makeLet <$> token TokLet <*> letInMain)
        <|> try (makeSequential <$> (comp <* token TokSemicolon) <*> letin)
        <|> lam
      where
        makeLet locFirst (eMain, locLast) =
          Expr (mergeSpan locFirst locLast) eMain

        makeSequential e1@(Expr locFirst _) e2@(Expr locLast _) =
          Expr (mergeSpan locFirst locLast) (Sequential e1 e2)

    letInMain :: P (ExprMain, Span)
    letInMain =
      try (makeLetTupleIn <$> paren ((,) <$> (noLoc boundIdent <* token TokComma) <*> noLoc boundIdent) <*> (token TokEqual *> letin) <*> (token TokIn *> letin))
        <|> (makeLetIn <$> noLoc boundIdent <*> many lamBinder <*> (token TokEqual *> letin) <*> (token TokIn *> letin))
        <|> (makeLetRecIn <$> (token TokRec *> noLoc boundIdent) <*> many lamBinder <*> (token TokColon *> typeExpr) <*> (token TokEqual *> letin) <*> (token TokIn *> letin))
        <|> (makeLetOpenIn <$> (token TokOpen *> noLoc upper) <*> (token TokIn *> letin))
      where
        makeLetTupleIn (Located _ (x1, x2)) e1 e2@(Expr locLast _) = (LetTupleIn x1 x2 e1 e2, locLast)
        makeLetIn x params e1 e2@(Expr locLast _) = (LetIn x params e1 e2, locLast)
        makeLetRecIn x params tye e1 e2@(Expr locLast _) = (LetRecIn x params tye e1 e2, locLast)
        makeLetOpenIn m e@(Expr locLast _) = (LetOpenIn m e, locLast)

typeExpr :: P TypeExpr
typeExpr = fun
  where
    atom :: P TypeExpr
    atom =
      (makeNamed <$> upper)
        <|> (makeTypeVar <$> typeVar)
        <|> try (makeRefinement <$> paren ((,,) <$> (noLoc boundIdent <* token TokColon) <*> (fun <* token TokBar) <*> expr))
        <|> (makeEnclosed <$> paren fun)
      where
        makeNamed (Located loc t) = TypeExpr loc (TyName t [])
        makeTypeVar (Located loc a) = TypeExpr loc (TyVar a)
        makeRefinement (Located loc (x, tye, e)) = TypeExpr loc (TyRefinement x tye e)
        makeEnclosed (Located loc (TypeExpr _ tyeMain)) = TypeExpr loc tyeMain

    staged :: P TypeExpr
    staged =
      (makeTyCode <$> token TokBracket <*> staged)
        <|> atom
      where
        makeTyCode loc1 tye@(TypeExpr loc2 _) =
          TypeExpr (mergeSpan loc1 loc2) (TyCode tye)

    app :: P TypeExpr
    app =
      try (makeTyName <$> upper <*> some argForType)
        <|> staged
      where
        makeTyName (Located locFirst t) args =
          let loc =
                mergeSpan locFirst $
                  case NonEmpty.last args of
                    ExprArgPersistent (Expr locLast _) -> locLast
                    ExprArgNormal (Expr locLast _) -> locLast
                    TypeArg (TypeExpr locLast _) -> locLast
           in TypeExpr loc (TyName t (NonEmpty.toList args))

    argForType :: P ArgForType
    argForType =
      (ExprArgPersistent <$> (token TokPersistent *> exprAtom))
        <|> try (ExprArgNormal <$> exprAtom)
        <|> (TypeArg <$> atom)

    prod :: P TypeExpr
    prod =
      try (makeProduct <$> app <*> (token TokProd *> app))
        <|> app
      where
        makeProduct ty1@(TypeExpr loc1 _) ty2@(TypeExpr loc2 _) =
          TypeExpr (mergeSpan loc1 loc2) (TyProduct ty1 ty2)

    fun :: P TypeExpr
    fun =
      try (makeTyArrow <$> funDom <*> (token TokArrow *> fun))
        <|> prod
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
      (makeFunDom False <$> brace ((,) <$> (noLoc lower <* token TokColon) <*> fun))
        <|> try (makeFunDom True <$> paren ((,) <$> (noLoc lower <* token TokColon) <*> fun))
        <|> ((Nothing,) <$> prod)
      where
        makeFunDom isMandatory (Located loc (x, tyeDom)) = (Just (isMandatory, loc, x), tyeDom)

bind :: P Bind
bind =
  (makeBindVal <$> token TokVal <*> valBinder <*> bindVal)
    <|> (makeBindModule <$> token TokModule <*> noLoc upper <*> (token TokEqual *> token TokStruct *> many bind) <*> token TokEnd)
  where
    makeBindVal locFirst (stage, x) (bv, locLast) =
      Bind (mergeSpan locFirst locLast) (BindVal stage x bv)

    makeBindModule locFirst m binds locLast =
      Bind (mergeSpan locFirst locLast) (BindModule m binds)

valBinder :: P (Stage, Var)
valBinder =
  ((Stage0,) <$> (token TokEscape *> noLoc boundIdent))
    <|> ((StagePers,) <$> (token TokPersistent *> noLoc boundIdent))
    <|> ((Stage1,) <$> noLoc boundIdent)

bindVal :: P (BindVal, Span)
bindVal =
  (makeBindValExternal <$> many (noLoc typeVar) <*> (token TokColon *> typeExpr) <*> (token TokExternal *> external))
    <|> (makeBindValNormal <$> (token TokEqual *> expr))
  where
    makeBindValExternal tyvars ty (Located locLast ext) = (BindValExternal tyvars ty ext, locLast)
    makeBindValNormal e@(Expr locLast _) = (BindValNormal e, locLast)

external :: P (Located External)
external =
  paren (field `sepBy` token TokComma)
  where
    field :: P (Text, Text)
    field = (,) <$> (noLoc lower <* token TokEqual) <*> noLoc string

parse :: P a -> SourceSpec -> Text -> Either FrontError a
parse p sourceSpec source = do
  locatedTokens <- mapLeft FrontLexingError $ Token.lex source
  mapLeft FrontParseError $ runParser p sourceSpec locatedTokens

parseExpr :: SourceSpec -> Text -> Either FrontError Expr
parseExpr = parse (expr <* eof)

parseTypeExpr :: SourceSpec -> Text -> Either FrontError TypeExpr
parseTypeExpr = parse (typeExpr <* eof)

parseBinds :: SourceSpec -> Text -> Either FrontError [Bind]
parseBinds = parse (many bind <* eof)
