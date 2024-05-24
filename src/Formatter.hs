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

instance Disp Ass0Expr where
  dispGen req = \case
    A0Literal lit -> disp lit
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
