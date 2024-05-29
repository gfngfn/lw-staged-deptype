module Formatter where

import Data.Text (Text)
import Data.Text qualified as Text
import Evaluator qualified
import Prettyprinter
import Prettyprinter.Render.Text
import Syntax
import TypeError
import Vector qualified

type Ann = ()

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

commaSep :: [Doc Ann] -> Doc Ann
commaSep = sep . punctuate comma

deepenParen :: Doc Ann -> Doc Ann
deepenParen doc = "(" <> nest 2 doc <> ")"

deepenParenCommaSep :: [Doc Ann] -> Doc Ann
deepenParenCommaSep docs = "(" <> nest 2 (sep (punctuate comma (addFirstBreak docs))) <> ")"

addFirstBreak :: [Doc Ann] -> [Doc Ann]
addFirstBreak [] = []
addFirstBreak (doc : docs) = (line' <> doc) : docs

instance Disp Text where
  dispGen _ = pretty

instance Disp Int where
  dispGen _ = pretty

instance Disp Symbol where
  dispGen _ symb = disp (symbolToVar symb)

instance Disp Literal where
  dispGen _ = \case
    LitInt n -> pretty n
    LitVec v -> encloseSep ("[|" <> space) (space <> "|]") (";" <> softline) (disp <$> Vector.toList v)

instance Disp BuiltIn where
  dispGen _ = \case
    BIAdd x1 x2 -> "ADD(" <> disp x1 <> "," <+> disp x2 <> ")"
    BIGenVadd x -> "GEN_VADD(" <> disp x <> ")"
    BIGenVconcat x1 x2 -> "GEN_VCONCAT(" <> disp x1 <> "," <+> disp x2 <> ")"
    BIVadd n x1 x2 -> "VADD@{" <> disp n <> "}(" <> disp x1 <> "," <+> disp x2 <> ")"
    BIVconcat m n x1 x2 -> "VCONCAT@{" <> disp m <> "," <+> disp n <> "}(" <> disp x1 <> "," <+> disp x2 <> ")"

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
      "&" <> dispGen Atomic a1e1
    A0TyEqAssert ty0eq a0e0 ->
      let (a0tye1, a0tye2) = decomposeType0Equality ty0eq
          doc = group ("{" <> disp a0tye1 <+> "▷" <+> disp a0tye2 <> "}" <> line <> disp a0e0)
       in if req <= FunDomain then deepenParen doc else doc

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
      "~" <> dispGen Atomic a0e1

instance Disp Ass0PrimType where
  dispGen req = \case
    A0TyInt -> "Int"
    A0TyBool -> "Bool"
    A0TyVec n ->
      let doc = "Vec" <+> disp n
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
      "&" <> dispGen Atomic a1tye1

instance Disp Ass1PrimType where
  dispGen req = \case
    A1TyInt -> "Int"
    A1TyBool -> "Bool"
    A1TyVec a0e ->
      let doc = "Vec %" <> dispGen Atomic a0e
       in if req <= Atomic then deepenParen doc else doc

instance Disp Ass1TypeExpr where
  dispGen req = \case
    A1TyPrim a1tyPrim -> disp a1tyPrim
    A1TyArrow a1tye1 a1tye2 ->
      let doc = group (dispGen FunDomain a1tye1 <> " ->" <> line <> disp a1tye2)
       in if req <= FunDomain then deepenParen doc else doc

instance Disp TypeError where
  dispGen _ = \case
    UnboundVar x ->
      "Unbound variable" <+> disp x
    NotAStage0Var x ->
      "Not a stage-0 variable:" <+> disp x
    NotAStage1Var x ->
      "Not a stage-1 variable:" <+> disp x
    UnknownTypeOrInvalidArityAtStage0 tyName n ->
      "Unknown type or invalid arity (at stage 0):" <+> disp tyName <> "," <+> disp n
    UnknownTypeOrInvalidArityAtStage1 tyName n ->
      "Unknown type or invalid arity (at stage 1):" <+> disp tyName <> "," <+> disp n
    NotAnIntTypedArgOfVecAtStage1 a0tye ->
      "The argument expression of Vec is not Int-typed:" <+> disp a0tye
    TypeContradictionAtStage0 a0tye1 a0tye2 ->
      "Type contradiction at stage 0. left:" <> hardline
        <> disp a0tye1 <> "," <> hardline
        <> "right:" <> hardline
        <> disp a0tye2
    TypeContradictionAtStage1 a1tye1 a1tye2 ->
      "Type contradiction at stage 1. left:" <> hardline
        <> disp a1tye1 <> "," <> hardline
        <> "right:" <> hardline
        <> disp a1tye2
    NotAFunctionTypeForStage0 a0tye ->
      "Not a function type (for stage 0): " <+> disp a0tye
    NotAFunctionTypeForStage1 a1tye ->
      "Not a function type (for stage 1): " <+> disp a1tye
    NotACodeType a0tye ->
      "Not a code type:" <+> disp a0tye
    CannotUseEscapeAtStage0 ->
      "Cannot use Escape (~) at stage 0"
    CannotUseBracketAtStage1 ->
      "Cannot use Bracket (&) at stage 1"
    FunctionTypeCannotBeDependentAtStage1 x ->
      "Function types cannot be dependent at stage 1:" <+> disp x
    CannotUseCodeTypeAtStage1 ->
      "Cannot use code types at stage 1"
    CannotUsePersistentArgAtStage0 ->
      "Cannot use persistent arguments at stage 0"
    CannotUseNormalArgAtStage1 ->
      "Cannot use normal arguments at stage 1"

instance Disp Ass0Val where
  dispGen req = \case
    A0ValLiteral lit ->
      disp lit
    A0ValLam (x, a0tyv1) a0v2 _env ->
      let doc = group ("λ" <> disp x <+> ":" <+> disp a0tyv1 <> "." <> nest 2 (line <> disp a0v2))
       in if req <= FunDomain then deepenParen doc else doc
    A0ValBracket a1v1 ->
      "&" <> dispGen Atomic a1v1

instance Disp Ass1ValConst where
  dispGen _ = \case
    A1ValConstVadd n -> "vadd@{" <> disp n <> "}"
    A1ValConstVconcat m n -> "vconcat@{" <> disp m <> "," <+> disp n <> "}"

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
    A0TyValArrow (xOpt, a0tyv1) a0tye2 ->
      let docDom =
            case xOpt of
              Just x -> "(" <> disp x <+> ":" <+> disp a0tyv1 <> ")"
              Nothing -> dispGen FunDomain a0tyv1
          doc =
            group (docDom <> " ->" <> line <> disp a0tye2)
       in if req <= FunDomain then deepenParen doc else doc
    A0TyValCode a1tyv1 ->
      "&" <> dispGen Atomic a1tyv1

instance Disp Ass1TypeVal where
  dispGen req = \case
    A1TyValPrim a1tyvPrim ->
      case a1tyvPrim of
        A1TyValInt -> "Int"
        A1TyValBool -> "Bool"
        A1TyValVec a0v ->
          let doc = "Vec %" <> dispGen Atomic a0v
           in if req <= Atomic then deepenParen doc else doc
    A1TyValArrow a1tyv1 a1tyv2 ->
      let doc = group (dispGen FunDomain a1tyv1 <> " ->" <> line <> disp a1tyv2)
       in if req <= FunDomain then deepenParen doc else doc

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
    Evaluator.AssertionFailure a1tyv1 a1tyv2 ->
      "Assertion failure. left:" <> hardline
        <> disp a1tyv1 <> "," <> hardline
        <> "right:" <> hardline
        <> disp a1tyv2
