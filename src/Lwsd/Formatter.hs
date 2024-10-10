module Lwsd.Formatter
  ( Disp (..),
    render,
    putRenderedLines,
    renderAtStage0,
    putRenderedLinesAtStage0,
    putRenderedLinesAtStage1,
  )
where

import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Lwsd.Evaluator qualified as Evaluator
import Lwsd.Syntax
import Lwsd.TypeError
import Prettyprinter
import Prettyprinter.Render.Terminal
import Surface.BindingTime.Analyzer qualified as Bta
import Surface.BindingTime.Core qualified as Bta
import Surface.BindingTime.Stager qualified as Bta
import Surface.Syntax qualified as Surface
import Util.LocationInFile (LocationInFile (LocationInFile), SpanInFile (..))
import Util.Matrix qualified as Matrix
import Util.Vector qualified as Vector
import Prelude

type Ann = AnsiStyle

bindingTime0Style :: Doc Ann -> Doc Ann
bindingTime0Style = annotate (color Green)

bindingTime1Style :: Doc Ann -> Doc Ann
bindingTime1Style = annotate (color Red)

stage0Style :: Doc Ann -> Doc Ann
stage0Style = annotate (color Cyan) -- reAnnotate (<> color Cyan)

stage1Style :: Doc Ann -> Doc Ann
stage1Style = annotate (color Magenta) -- reAnnotate (<> color Magenta)

stagingOperatorStyle :: Doc Ann -> Doc Ann
stagingOperatorStyle = annotate (color Yellow)

assertionStyle :: Doc Ann -> Doc Ann
assertionStyle = id -- annotate (bgColorDull Blue)

data Associativity
  = Atomic
  | FunDomain
  | Outermost
  deriving (Eq, Ord) -- `Atomic` is the smallest

class Disp a where
  dispGen :: Associativity -> a -> Doc Ann
  disp :: a -> Doc Ann
  disp = dispGen Outermost

renderDoc :: Int -> Doc Ann -> Text
renderDoc wid doc =
  renderStrict $
    layoutSmart (LayoutOptions {layoutPageWidth = AvailablePerLine wid 1.0}) doc

render :: (Disp a) => Int -> a -> Text
render wid = renderDoc wid . disp

putRenderedLines :: (Disp a) => Int -> a -> IO ()
putRenderedLines wid x =
  putStrLn $ Text.unpack $ render wid x

renderAtStage0 :: (Disp a) => Int -> a -> Text
renderAtStage0 wid = renderDoc wid . stage0Style . disp

putRenderedLinesAtStage0 :: (Disp a) => Int -> a -> IO ()
putRenderedLinesAtStage0 wid x =
  putStrLn $ Text.unpack $ renderAtStage0 wid x

renderAtStage1 :: (Disp a) => Int -> a -> Text
renderAtStage1 wid = renderDoc wid . stage1Style . disp

putRenderedLinesAtStage1 :: (Disp a) => Int -> a -> IO ()
putRenderedLinesAtStage1 wid x =
  putStrLn $ Text.unpack $ renderAtStage1 wid x

commaSep :: [Doc Ann] -> Doc Ann
commaSep = sep . punctuate comma

disps :: (Disp a) => [a] -> Doc Ann
disps [] = mempty
disps (first : rest) = List.foldl' (\doc x -> doc <> "," <+> disp x) (disp first) rest

deepenParen :: Doc Ann -> Doc Ann
deepenParen doc = "(" <> nest 2 doc <> ")"

instance Disp Text where
  dispGen _ = pretty

instance Disp String where
  dispGen _ = pretty

instance Disp Int where
  dispGen _ = pretty

instance Disp Symbol where
  dispGen _ symb = disp (symbolToVar symb)

instance Disp Literal where
  dispGen _ = \case
    LitInt n -> pretty n
    LitVec ns -> encloseSep ("[|" <> space) (space <> "|]") (";" <> softline) (disp <$> ns)
    LitMat nss -> encloseSep ("[#" <> space) (space <> "#]") (";" <> softline) (dispRow <$> nss)
    where
      dispRow :: [Int] -> Doc Ann
      dispRow row = commaSep (disp <$> row)

instance Disp (ExprF ann) where
  dispGen req (Expr _ann exprMain) = dispGen req exprMain

instance Disp (ExprMainF ann) where
  dispGen req = \case
    Literal lit -> dispGen req lit
    Var x -> disp x
    Lam (x, tye1) e2 ->
      let doc = group ("λ" <> disp x <+> ":" <+> disp tye1 <> "." <> nest 2 (line <> disp e2))
       in if req <= FunDomain then deepenParen doc else doc
    App e1 e2 ->
      let doc = group (dispGen FunDomain e1 <> nest 2 (line <> dispGen Atomic e2))
       in if req <= Atomic then deepenParen doc else doc
    LetIn x e1 e2 ->
      let doc = group ("let" <+> disp x <+> "=" <+> disp e1 <+> "in" <+> disp e2)
       in if req <= FunDomain then deepenParen doc else doc
    Bracket e1 ->
      stagingOperatorStyle "&" <> stage1Style (dispGen Atomic e1)
    Escape e1 ->
      stagingOperatorStyle "~" <> stage0Style (dispGen Atomic e1)

instance Disp (TypeExprF ann) where
  dispGen req (TypeExpr _ann typeExprMain) = dispGen req typeExprMain

instance Disp (TypeExprMainF ann) where
  dispGen req = \case
    TyName tyName args ->
      case args of
        [] -> disp tyName
        _ : _ ->
          let doc = List.foldl' (<+>) (disp tyName) (map (dispGen Atomic) args)
           in if req <= Atomic then deepenParen doc else doc
    TyArrow (xOpt, tye1) tye2 ->
      let docDom =
            case xOpt of
              Just x -> "(" <> disp x <+> ":" <+> disp tye1 <> ")"
              Nothing -> dispGen FunDomain tye1
          doc =
            group (docDom <> " ->" <> line <> disp tye2)
       in if req <= FunDomain then deepenParen doc else doc
    TyCode tye1 ->
      stagingOperatorStyle "&" <> stage1Style (dispGen Atomic tye1)

instance Disp (ArgForTypeF ann) where
  dispGen req = \case
    PersistentArg e -> stagingOperatorStyle "%" <> stage0Style (dispGen Atomic e)
    NormalArg e -> dispGen req e

instance Disp BuiltIn where
  dispGen _ = \case
    BIAdd x1 x2 -> "ADD(" <> disps [x1, x2] <> ")"
    BISub x1 x2 -> "SUB(" <> disps [x1, x2] <> ")"
    BIMult x1 x2 -> "MULT(" <> disps [x1, x2] <> ")"
    BIGenVadd x -> "GEN_VADD(" <> disp x <> ")"
    BIGenVconcat x1 x2 -> "GEN_VCONCAT(" <> disps [x1, x2] <> ")"
    BIGenMtranspose x1 x2 -> "GEN_MTRANSPOSE(" <> disps [x1, x2] <> ")"
    BIGenMmult x1 x2 x3 -> "GEN_MMULT(" <> disps [x1, x2, x3] <> ")"
    BIGenMconcatVert x1 x2 x3 -> "GEN_MCONCAT_VERT(" <> disps [x1, x2, x3] <> ")"
    BIVadd n x1 x2 -> "VADD@{" <> disp n <> "}(" <> disps [x1, x2] <> ")"
    BIVconcat m n x1 x2 -> "VCONCAT@{" <> disps [m, n] <> "}(" <> disps [x1, x2] <> ")"
    BIMtranspose m n x1 -> "MTRANSPOSE@{" <> disps [m, n] <> "}(" <> disp x1 <> ")"
    BIMmult k m n x1 x2 -> "MMULT@{" <> disps [k, m, n] <> "}(" <> disps [x1, x2] <> "}"
    BIMconcatVert m1 m2 n x1 x2 -> "MCONCAT_VERT@{" <> disps [m1, m2, n] <> "}(" <> disps [x1, x2] <> ")"

instance Disp Surface.Literal where
  dispGen _ = \case
    Surface.LitInt n -> pretty n
    Surface.LitVec ns -> encloseSep ("[|" <> space) (space <> "|]") (";" <> softline) (disp <$> ns)
    Surface.LitMat nss -> encloseSep ("[#" <> space) (space <> "#]") (";" <> softline) (dispRow <$> nss)
    where
      dispRow :: [Int] -> Doc Ann
      dispRow row = commaSep (disp <$> row)

instance Disp Surface.Expr where
  dispGen req (Surface.Expr _ann exprMain) = dispGen req exprMain

instance Disp Surface.ExprMain where
  dispGen req = \case
    Surface.Literal lit -> dispGen req lit
    Surface.Var x -> disp x
    Surface.Lam (x, tye1) e2 ->
      let doc = group ("λ" <> disp x <+> ":" <+> disp tye1 <> "." <> nest 2 (line <> disp e2))
       in if req <= FunDomain then deepenParen doc else doc
    Surface.App e1 e2 ->
      let doc = group (dispGen FunDomain e1 <> nest 2 (line <> dispGen Atomic e2))
       in if req <= Atomic then deepenParen doc else doc
    Surface.LetIn x e1 e2 ->
      let doc = group ("let" <+> disp x <+> "=" <+> disp e1 <+> "in" <+> disp e2)
       in if req <= FunDomain then deepenParen doc else doc

instance Disp Surface.TypeExpr where
  dispGen req (Surface.TypeExpr _ann typeExprMain) = dispGen req typeExprMain

instance Disp Surface.TypeExprMain where
  dispGen req = \case
    Surface.TyName tyName args ->
      case args of
        [] -> disp tyName
        _ : _ ->
          let doc = List.foldl' (<+>) (disp tyName) (map (dispGen Atomic) args)
           in if req <= Atomic then deepenParen doc else doc
    Surface.TyArrow (xOpt, tye1) tye2 ->
      let docDom =
            case xOpt of
              Just x -> "(" <> disp x <+> ":" <+> disp tye1 <> ")"
              Nothing -> dispGen FunDomain tye1
          doc =
            group (docDom <> " ->" <> line <> disp tye2)
       in if req <= FunDomain then deepenParen doc else doc

dispRowContents :: [Int] -> Doc Ann
dispRowContents row = commaSep (disp <$> row)

instance Disp AssLiteral where
  dispGen _ = \case
    ALitInt n -> pretty n
    ALitVec v -> encloseSep ("[|" <> space) (space <> "|]") (";" <> softline) (disp <$> Vector.toList v)
    ALitMat m -> encloseSep ("[#" <> space) (space <> "#]") (";" <> softline) (dispRowContents <$> Matrix.toRows m)

instance Disp Ass0Expr where
  dispGen req = \case
    A0Literal lit -> disp lit
    A0AppBuiltIn bi -> disp bi
    A0Var y -> disp y
    A0Lam (y, a0tye1) a0e2 ->
      let doc = group ("λ" <> disp y <+> ":" <+> disp a0tye1 <> "." <> nest 2 (line <> disp a0e2))
       in if req <= FunDomain then deepenParen doc else doc
    A0App a0e1 a0e2 ->
      let doc = group (dispGen FunDomain a0e1 <> nest 2 (line <> dispGen Atomic a0e2))
       in if req <= Atomic then deepenParen doc else doc
    A0Bracket a1e1 ->
      stagingOperatorStyle "&" <> stage1Style (dispGen Atomic a1e1)
    A0TyEqAssert _loc ty1eq ->
      let (a1tye1, a1tye2) = decomposeType1Equation ty1eq
       in group
            ( assertionStyle
                ( "{"
                    <> stagingOperatorStyle "&"
                    <> stage1Style (dispGen Atomic a1tye1)
                    <+> "=>"
                    <+> stagingOperatorStyle "&"
                    <> stage1Style (dispGen Atomic a1tye2)
                    <> "}"
                )
            )

instance Disp Ass1Expr where
  dispGen req = \case
    A1Literal lit -> disp lit
    A1Var x -> disp x
    A1Lam (x, a1tye1) a1e2 ->
      let doc = "λ" <> disp x <+> ":" <+> disp a1tye1 <> "." <+> disp a1e2
       in if req <= FunDomain then deepenParen doc else doc
    A1App a1e1 a1e2 ->
      let doc = dispGen FunDomain a1e1 <+> dispGen Atomic a1e2
       in if req <= Atomic then deepenParen doc else doc
    A1Escape a0e1 ->
      stagingOperatorStyle "~" <> stage0Style (dispGen Atomic a0e1)

instance Disp Ass0PrimType where
  dispGen req = \case
    A0TyInt -> "Int"
    A0TyBool -> "Bool"
    A0TyVec n ->
      let doc = "Vec" <+> disp n
       in if req <= Atomic then deepenParen doc else doc
    A0TyMat m n ->
      let doc = "Mat" <+> disp m <+> disp n
       in if req <= Atomic then deepenParen doc else doc

instance Disp Ass0TypeExpr where
  dispGen req = \case
    A0TyPrim a0tyPrim -> disp a0tyPrim
    A0TyArrow (xOpt, a0tye1) a0tye2 ->
      let docDom =
            case xOpt of
              Just x -> "(" <> disp x <+> ":" <+> disp a0tye1 <> ")"
              Nothing -> dispGen FunDomain a0tye1
          doc =
            group (docDom <> " ->" <> line <> disp a0tye2)
       in if req <= FunDomain then deepenParen doc else doc
    A0TyCode a1tye1 ->
      stagingOperatorStyle "&" <> stage1Style (dispGen Atomic a1tye1)

instance Disp Ass1PrimType where
  dispGen req = \case
    A1TyInt -> "Int"
    A1TyBool -> "Bool"
    A1TyVec a0e ->
      let doc = "Vec" <+> stagingOperatorStyle "%" <> stage0Style (dispGen Atomic a0e)
       in if req <= Atomic then deepenParen doc else doc
    A1TyMat a0e1 a0e2 ->
      let doc =
            "Mat"
              <+> stagingOperatorStyle "%"
              <> stage0Style (dispGen Atomic a0e1)
              <+> stagingOperatorStyle "%"
              <> stage0Style (dispGen Atomic a0e2)
       in if req <= Atomic then deepenParen doc else doc

instance Disp Ass1TypeExpr where
  dispGen req = \case
    A1TyPrim a1tyPrim -> dispGen req a1tyPrim
    A1TyArrow a1tye1 a1tye2 ->
      let doc = group (dispGen FunDomain a1tye1 <> " ->" <> line <> disp a1tye2)
       in if req <= FunDomain then deepenParen doc else doc

instance Disp Matrix.ConstructionError where
  dispGen _ = \case
    Matrix.EmptyRow -> "contains an empty row"
    Matrix.InconsistencyOfRowLength row1 row2 ->
      "two rows have different lengths. one:"
        <> hardline
        <> dispRowContents row1
        <> hardline
        <> "another:"
        <> hardline
        <> dispRowContents row2

instance Disp TypeError where
  dispGen _ = \case
    UnboundVar spanInFile x ->
      "Unbound variable" <+> disp x <+> disp spanInFile
    NotAStage0Var spanInFile x ->
      "Not a stage-0 variable:" <+> disp x <+> disp spanInFile
    NotAStage1Var spanInFile x ->
      "Not a stage-1 variable:" <+> disp x <+> disp spanInFile
    UnknownTypeOrInvalidArityAtStage0 spanInFile tyName n ->
      "Unknown type or invalid arity (at stage 0):" <+> disp tyName <> "," <+> disp n <+> disp spanInFile
    UnknownTypeOrInvalidArityAtStage1 spanInFile tyName n ->
      "Unknown type or invalid arity (at stage 1):" <+> disp tyName <> "," <+> disp n <+> disp spanInFile
    NotAnIntLitArgAtStage0 spanInFile a0e ->
      "An argument expression at stage 0 is not an integer literal:" <+> stage0Style (disp a0e) <+> disp spanInFile
    NotAnIntTypedArgAtStage1 spanInFile a0tye ->
      "An argument expression at stage 1 is not Int-typed:" <+> stage0Style (disp a0tye) <+> disp spanInFile
    TypeContradictionAtStage0 spanInFile a0tye1 a0tye2 ->
      "Type contradiction at stage 0"
        <+> disp spanInFile
        <> hardline
        <> "left:"
        <> nest 2 (hardline <> stage0Style (disp a0tye1))
        <> hardline
        <> "right:"
        <> nest 2 (hardline <> stage0Style (disp a0tye2))
    TypeContradictionAtStage1 spanInFile a1tye1 a1tye2 ->
      "Type contradiction at stage 1"
        <+> disp spanInFile
        <> hardline
        <> "left:"
        <> nest 2 (hardline <> stage1Style (disp a1tye1))
        <> hardline
        <> "right:"
        <> nest 2 (hardline <> stage1Style (disp a1tye2))
    NotAFunctionTypeForStage0 spanInFile a0tye ->
      "Not a function type (at stage 0):" <+> stage0Style (disp a0tye) <+> disp spanInFile
    NotAFunctionTypeForStage1 spanInFile a1tye ->
      "Not a function type (at stage 1):" <+> stage1Style (disp a1tye) <+> disp spanInFile
    NotACodeType spanInFile a0tye ->
      "Not a code type:" <+> stage0Style (disp a0tye) <+> disp spanInFile
    CannotUseEscapeAtStage0 spanInFile ->
      "Cannot use Escape (~) at stage 0" <+> disp spanInFile
    CannotUseBracketAtStage1 spanInFile ->
      "Cannot use Bracket (&) at stage 1" <+> disp spanInFile
    FunctionTypeCannotBeDependentAtStage1 spanInFile x ->
      "Function types cannot be dependent at stage 1:" <+> disp x <+> disp spanInFile
    CannotUseCodeTypeAtStage1 spanInFile ->
      "Cannot use code types at stage 1" <+> disp spanInFile
    CannotUsePersistentArgAtStage0 spanInFile ->
      "Cannot use persistent arguments at stage 0" <+> disp spanInFile
    CannotUseNormalArgAtStage1 spanInFile ->
      "Cannot use normal arguments at stage 1" <+> disp spanInFile
    VarOccursFreelyInAss0Type spanInFile x a0tye ->
      "Variable" <+> disp x <+> "occurs in stage-0 type" <+> stage0Style (disp a0tye) <+> disp spanInFile
    VarOccursFreelyInAss1Type spanInFile x a1tye ->
      "Variable" <+> disp x <+> "occurs in stage-1 type" <+> stage1Style (disp a1tye) <+> disp spanInFile
    InvalidMatrixLiteral spanInFile e ->
      "Invalid matrix literal;" <+> disp e <+> disp spanInFile

instance Disp Ass0Val where
  dispGen req = \case
    A0ValLiteral lit ->
      disp lit
    A0ValLam (x, a0tyv1) a0v2 _env ->
      let doc = group ("λ" <> disp x <+> ":" <+> disp a0tyv1 <> "." <> nest 2 (line <> disp a0v2))
       in if req <= FunDomain then deepenParen doc else doc
    A0ValBracket a1v1 ->
      stagingOperatorStyle "&" <> stage1Style (dispGen Atomic a1v1)

instance Disp Ass1ValConst where
  dispGen _ = \case
    A1ValConstVadd n -> "vadd@{" <> disp n <> "}"
    A1ValConstVconcat m n -> "vconcat@{" <> disps [m, n] <> "}"
    A1ValConstMtranspose m n -> "mtranspose@{" <> disps [m, n] <> "}"
    A1ValConstMmult k m n -> "mmult@{" <> disps [k, m, n] <> "}"
    A1ValConstMconcatVert m1 m2 n -> "mconcat_vert@{" <> disps [m1, m2, n] <> "}"

instance Disp Ass1Val where
  dispGen req = \case
    A1ValLiteral lit -> disp lit
    A1ValConst c -> disp c
    A1ValVar symb -> disp symb
    A1ValLam (symb, a1tyv1) a1v2 ->
      let doc = "λ" <> disp symb <+> ":" <+> disp a1tyv1 <> "." <+> disp a1v2
       in if req <= FunDomain then deepenParen doc else doc
    A1ValApp a1v1 a1v2 ->
      let doc = group (dispGen FunDomain a1v1 <> nest 2 (line <> dispGen Atomic a1v2))
       in if req <= Atomic then deepenParen doc else doc

instance Disp Ass0TypeVal where
  dispGen req = \case
    A0TyValPrim a0tyvPrim ->
      case a0tyvPrim of
        A0TyValInt -> "Int"
        A0TyValBool -> "Bool"
        A0TyValVec n ->
          let doc = "Vec" <+> disp n
           in if req <= Atomic then deepenParen doc else doc
        A0TyValMat m n ->
          let doc = "Mat" <+> disp m <+> disp n
           in if req <= Atomic then deepenParen doc else doc
    A0TyValArrow (xOpt, a0tyv1) a0tye2 ->
      let docDom =
            case xOpt of
              Just x -> "(" <> disp x <+> ":" <+> disp a0tyv1 <> ")"
              Nothing -> dispGen FunDomain a0tyv1
          doc =
            group (docDom <> " ->" <> line <> disp a0tye2)
       in if req <= FunDomain then deepenParen doc else doc
    A0TyValCode a1tyv1 ->
      stagingOperatorStyle "&" <> stage1Style (dispGen Atomic a1tyv1)

instance Disp Ass1TypeVal where
  dispGen req = \case
    A1TyValPrim a1tyvPrim ->
      case a1tyvPrim of
        A1TyValInt -> "Int"
        A1TyValBool -> "Bool"
        A1TyValVec a0v ->
          let doc = "Vec" <+> stagingOperatorStyle "%" <> stage0Style (dispGen Atomic a0v)
           in if req <= Atomic then deepenParen doc else doc
        A1TyValMat a0v1 a0v2 ->
          let doc =
                "Mat"
                  <+> stagingOperatorStyle "%"
                  <> stage0Style (dispGen Atomic a0v1)
                  <+> stagingOperatorStyle "&"
                  <> stage0Style (dispGen Atomic a0v2)
           in if req <= Atomic then deepenParen doc else doc
    A1TyValArrow a1tyv1 a1tyv2 ->
      let doc = group (dispGen FunDomain a1tyv1 <> " ->" <> line <> disp a1tyv2)
       in if req <= FunDomain then deepenParen doc else doc

instance Disp LocationInFile where
  dispGen _ (LocationInFile l c) =
    "line" <+> disp l <> ", column" <+> disp (c - 1)

instance Disp SpanInFile where
  dispGen _ (SpanInFile {startLocation, endLocation, contents}) =
    "(from"
      <+> disp startLocation
      <+> "to"
      <+> disp endLocation
      <> ")"
      <> maybe mempty makeLineText contents
    where
      makeLineText s =
        if startLine == endLine
          then hardline <> disp s <> hardline <> indentation <> hats
          else mempty
        where
          LocationInFile startLine startColumn = startLocation
          LocationInFile endLine endColumn = endLocation
          indentation = disp (replicate (startColumn - 1) ' ')
          hats = disp (replicate (endColumn - startColumn) '^')

instance Disp Evaluator.Bug where
  dispGen _ = \case
    Evaluator.UnboundVar x ->
      "Unbound variable:" <+> disp x
    Evaluator.NotAClosure a0v ->
      "Not a closure:" <+> disp a0v
    Evaluator.NotACodeValue a0v ->
      "Not a code value:" <+> disp a0v
    Evaluator.NotAnInteger Nothing a0v ->
      "Not an integer:" <+> disp a0v
    Evaluator.NotAnInteger (Just x) a0v ->
      "Not an integer:" <+> disp a0v <+> "(bound to:" <+> disp x <> ")"
    Evaluator.NotAVector x a0v ->
      "Not a vector:" <+> disp a0v <+> "(bound to:" <+> disp x <> ")"
    Evaluator.NotAMatrix x a0v ->
      "Not a matrix:" <+> disp a0v <+> "(bound to:" <+> disp x <> ")"
    Evaluator.FoundSymbol x symb ->
      "Expected a stage-0 value, but found a symbol:" <+> disp symb <+> "(bound to:" <+> disp x <> ")"
    Evaluator.FoundAss0Val x a0v ->
      "Expected a symbol, but found a stage-0 value:" <+> disp a0v <+> "(bound to:" <+> disp x <> ")"
    Evaluator.InconsistentAppBuiltIn builtin ->
      "Inconsistent application of a built-in function:" <+> disp (Text.pack (show builtin))

instance Disp Evaluator.EvalError where
  dispGen _ = \case
    Evaluator.Bug bug ->
      "Bug:" <+> disp bug
    Evaluator.AssertionFailure spanInFile a1tyv1 a1tyv2 ->
      "Assertion failure"
        <+> disp spanInFile
        <> hardline
        <> "left:"
        <> nest 2 (hardline <> disp a1tyv1)
        <> hardline
        <> "right:"
        <> nest 2 (hardline <> disp a1tyv2)

instance Disp Bta.AnalysisError where
  dispGen _ = \case
    Bta.UnboundVar spanInFile x ->
      "Unbound variable" <+> disp x <+> disp spanInFile
    Bta.NotAFunction spanInFile bity ->
      "Not a function type;" <+> disp (show bity) <+> disp spanInFile
    Bta.BindingTimeContradiction spanInFile ->
      "Binding-time contradiction" <+> disp spanInFile

instance Disp (Bta.BCExprF ann) where
  dispGen _ (Surface.Expr (btc, _ann) exprMain) =
    let (f, prefix) =
          case btc of
            Bta.BT0 -> (bindingTime0Style, "$0")
            Bta.BT1 -> (bindingTime1Style, "$1")
     in group (f (prefix <> "(") <> disp exprMain <> f ")")

instance Disp (Bta.BCExprMainF ann) where
  dispGen _ = \case
    Surface.Literal lit -> disp lit
    Surface.Var x -> disp x
    Surface.Lam (x, tye1) e2 -> group ("λ" <> disp x <+> ":" <+> disp tye1 <> "." <> nest 2 (line <> disp e2))
    Surface.App e1 e2 -> group (dispGen FunDomain e1 <> nest 2 (line <> dispGen Atomic e2))
    Surface.LetIn x e1 e2 -> group ("let" <+> disp x <+> "=" <+> disp e1 <+> "in" <+> disp e2)

instance Disp (Bta.BCTypeExprF ann) where
  dispGen _ (Surface.TypeExpr (btc, _ann) typeExprMain) =
    let (f, prefix) =
          case btc of
            Bta.BT0 -> (bindingTime0Style, "$0")
            Bta.BT1 -> (bindingTime1Style, "$1")
     in group (f (prefix <> "(") <> disp typeExprMain <> f ")")

instance Disp (Bta.BCTypeExprMainF ann) where
  dispGen _ = \case
    Surface.TyName tyName args ->
      case args of
        [] -> disp tyName
        _ : _ -> List.foldl' (<+>) (disp tyName) (map (dispGen Atomic) args)
    Surface.TyArrow (xOpt, tye1) tye2 ->
      let docDom =
            case xOpt of
              Just x -> "(" <> disp x <+> ":" <+> disp tye1 <> ")"
              Nothing -> dispGen FunDomain tye1
       in group (docDom <> " ->" <> line <> disp tye2)
