module Formatter where

import Data.Text (Text)
import Data.Text qualified as Text
import Prettyprinter
import Prettyprinter.Render.Text
import Syntax
import TypeError

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

renderDoc :: Doc Ann -> Text
renderDoc doc =
  renderStrict $
    layoutSmart (LayoutOptions {layoutPageWidth = AvailablePerLine 80 1.0}) doc

render :: (Disp a) => a -> Text
render = renderDoc . disp

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

instance Disp Literal where
  dispGen _ = \case
    LitInt n -> pretty n

instance Disp BuiltIn where
  dispGen _ = \case
    BIAdd x1 x2 -> "ADD" <+> disp x1 <+> disp x2
    BIGenVadd x -> "GEN_VADD" <+> disp x
    BIGenVconcat x1 x2 -> "GEN_VCONCAT" <+> disp x1 <+> disp x2

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
    A0AssertAndThen a0e1 a0e2 a0e0 ->
      let doc = group ("assert" <+> disp a0e1 <+> "=" <+> disp a0e2 <> ";" <> line <> disp a0e0)
       in if req <= FunDomain then deepenParen doc else doc

instance Disp Ass1Expr where
  dispGen req = \case
    A1Literal lit -> disp lit
    A1Var y -> disp y
    A1Lam (y, a1tye1) a1e2 ->
      let doc = "λ" <> disp y <+> ":" <+> disp a1tye1 <> "." <+> disp a1e2
       in if req <= FunDomain then deepenParen doc else doc
    A1App a1e1 a1e2 ->
      let doc = dispGen FunDomain a1e1 <+> dispGen Atomic a1e2
       in if req <= Atomic then deepenParen doc else doc
    A1Escape a0e1 ->
      "~" <> dispGen Atomic a0e1

instance Disp Ass0PrimType where
  dispGen _ = \case
    A0TyInt -> "Int"
    A0TyBool -> "Bool"

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
    other ->
      -- TODO: implement this
      disp (Text.pack (show other))

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
    A1ValVar x -> disp x
    A1ValLam (x, a1tyv1) a1v2 ->
      let doc = "λ" <> disp x <+> ":" <+> disp a1tyv1 <> "." <+> disp a1v2
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
