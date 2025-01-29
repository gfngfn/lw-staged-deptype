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
import Lwsd.SrcSyntax
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

deepenParenWhen :: Bool -> Doc Ann -> Doc Ann
deepenParenWhen b doc = if b then "(" <> nest 2 doc <> ")" else doc

dispNonrecLam :: (Disp var, Disp ty, Disp expr) => Associativity -> var -> ty -> expr -> Doc Ann
dispNonrecLam req x tye1 e2 =
  deepenParenWhen (req <= FunDomain) $
    group ("λ" <> disp x <+> ":" <+> disp tye1 <> "." <> nest 2 (line <> disp e2))

dispRecLam :: (Disp var, Disp ty, Disp expr) => Associativity -> var -> ty -> var -> ty -> expr -> Doc Ann
dispRecLam req f tyeRec x tye1 e2 =
  deepenParenWhen (req <= FunDomain) $
    group (docBinderF <+> docBinderX <> nest 2 (line <> disp e2))
  where
    docBinderF = "rec" <+> disp f <+> ":" <+> disp tyeRec <> "."
    docBinderX = "λ" <> disp x <+> ":" <+> disp tye1 <> "."

dispLamOpt :: (Disp var, Disp ty, Disp expr) => Associativity -> var -> ty -> expr -> Doc Ann
dispLamOpt req x tye1 e2 =
  deepenParenWhen (req <= FunDomain) $
    group ("λ{" <> disp x <+> ":" <+> disp tye1 <> "}." <> nest 2 (line <> disp e2))

dispApp :: (Disp expr) => Associativity -> expr -> expr -> Doc Ann
dispApp req e1 e2 =
  deepenParenWhen (req <= Atomic) $
    group (dispGen FunDomain e1 <> nest 2 (line <> dispGen Atomic e2))

dispAppOptGiven :: (Disp expr) => Associativity -> expr -> expr -> Doc Ann
dispAppOptGiven req e1 e2 =
  deepenParenWhen (req <= Atomic) $
    group (dispGen FunDomain e1 <> nest 2 (line <> "{" <> disp e2 <> "}"))

dispAppOptOmitted :: (Disp expr) => Associativity -> expr -> Doc Ann
dispAppOptOmitted req e1 =
  deepenParenWhen (req <= Atomic) $
    group (dispGen FunDomain e1 <> nest 2 (line <> "_"))

dispLetIn :: (Disp var, Disp expr) => Associativity -> var -> expr -> expr -> Doc Ann
dispLetIn req x e1 e2 =
  deepenParenWhen (req <= FunDomain) $
    group ("let" <+> disp x <+> "=" <> nest 2 (line <> disp e1) <+> "in" <> line <> disp e2)

dispLetInWithAnnot :: (Disp var, Disp ty, Disp expr) => Associativity -> var -> ty -> expr -> expr -> Doc Ann
dispLetInWithAnnot req x tye e1 e2 =
  deepenParenWhen (req <= FunDomain) $
    group ("let" <+> disp x <+> ":" <+> disp tye <+> "=" <> nest 2 (line <> disp e1) <+> "in" <> line <> disp e2)

dispLetOpenIn :: (Disp var, Disp expr) => Associativity -> var -> expr -> Doc Ann
dispLetOpenIn req m e =
  deepenParenWhen (req <= FunDomain) $
    group ("let open" <+> disp m <+> "in" <> line <> disp e)

dispIfThenElse :: (Disp expr) => Associativity -> expr -> expr -> expr -> Doc Ann
dispIfThenElse req e0 e1 e2 =
  deepenParenWhen (req <= FunDomain) $
    group (docIf <+> docThen <+> docElse)
  where
    docIf = "if" <> nest 2 (line <> disp e0)
    docThen = "then" <> nest 2 (line <> disp e1)
    docElse = "else" <> nest 2 (line <> disp e2)

dispAs :: (Disp expr, Disp ty) => Associativity -> expr -> ty -> Doc Ann
dispAs req e1 tye2 =
  deepenParenWhen (req <= FunDomain) $ group (disp e1 <+> "as" <+> disp tye2)

dispPersistent :: (Disp expr) => expr -> Doc Ann
dispPersistent e =
  stagingOperatorStyle "%" <> stage0Style (dispGen Atomic e)

dispPersistentListLiteral :: (Disp expr) => [expr] -> Doc Ann
dispPersistentListLiteral es =
  stagingOperatorStyle "%" <> stage0Style (dispListLiteral es)

dispBracket :: (Disp expr) => expr -> Doc Ann
dispBracket e =
  stagingOperatorStyle "&" <> stage1Style (dispGen Atomic e)

dispEscape :: (Disp expr) => expr -> Doc Ann
dispEscape e =
  stagingOperatorStyle "~" <> stage0Style (dispGen Atomic e)

dispListType :: (Disp ty) => Associativity -> ty -> Doc Ann
dispListType req tye =
  deepenParenWhen (req <= Atomic) $
    group ("List" <+> dispGen Atomic tye)

dispArrowType :: (Disp var, Disp ty1, Disp ty2) => Associativity -> Maybe var -> ty1 -> ty2 -> Doc Ann
dispArrowType req xOpt tye1 tye2 =
  deepenParenWhen (req <= FunDomain) $
    group (docDom <> " ->" <> line <> disp tye2)
  where
    docDom =
      case xOpt of
        Just x -> "(" <> disp x <+> ":" <+> disp tye1 <> ")"
        Nothing -> dispGen FunDomain tye1

dispNondepArrowType :: (Disp ty) => Associativity -> ty -> ty -> Doc Ann
dispNondepArrowType req =
  dispArrowType req (Nothing :: Maybe Text)

dispOptArrowType :: (Disp var, Disp ty1, Disp ty2) => Associativity -> var -> ty1 -> ty2 -> Doc Ann
dispOptArrowType req x tye1 tye2 =
  deepenParenWhen (req <= FunDomain) $
    group (docDom <> " ->" <> line <> disp tye2)
  where
    docDom = "{" <> disp x <+> ":" <+> disp tye1 <> "}"

dispListLiteral :: (Disp e) => [e] -> Doc Ann
dispListLiteral es =
  "[" <> disps es <> "]"

dispVectorLiteral :: [Int] -> Doc Ann
dispVectorLiteral ns =
  encloseSep ("[|" <> space) (space <> "|]") (";" <> softline) (disp <$> ns)

dispMatrixLiteral :: [[Int]] -> Doc Ann
dispMatrixLiteral nss =
  encloseSep ("[#" <> space) (space <> "#]") (";" <> softline) (dispRowContents <$> nss)

dispRowContents :: [Int] -> Doc Ann
dispRowContents row =
  commaSep (disp <$> row)

dispNameWithArgs :: Associativity -> Doc Ann -> (arg -> Doc Ann) -> [arg] -> Doc Ann
dispNameWithArgs req name dispArg args =
  case args of
    [] -> name
    _ : _ -> deepenParenWhen (req <= Atomic) (List.foldl' (<+>) name (map dispArg args))

dispLongName :: (Disp var) => [var] -> var -> Doc Ann
dispLongName ms x =
  foldr (\m doc -> disp m <> "." <> doc) (disp x) ms

instance Disp Text where
  dispGen _ = pretty

instance Disp String where
  dispGen _ = pretty

instance Disp Int where
  dispGen _ = pretty

instance Disp AssVar where
  dispGen _ (AssVar x) = disp x

instance Disp Symbol where
  dispGen _ symb = disp (symbolToVar symb)

instance (Disp e) => Disp (Literal e) where
  dispGen _ = \case
    LitInt n -> pretty n
    LitFloat r -> pretty r
    LitUnit -> "()"
    LitList es -> dispListLiteral es
    LitVec ns -> dispVectorLiteral ns
    LitMat nss -> dispMatrixLiteral nss

instance Disp (ExprF ann) where
  dispGen req (Expr _ann exprMain) = dispGen req exprMain

instance Disp (ExprMainF ann) where
  dispGen req = \case
    Literal lit -> dispGen req lit
    Var (ms, x) -> dispLongName ms x
    Lam Nothing (x, tye1) e2 -> dispNonrecLam req x tye1 e2
    Lam (Just (f, tyeRec)) (x, tye1) e2 -> dispRecLam req f tyeRec x tye1 e2
    App e1 e2 -> dispApp req e1 e2
    LamOpt (x, tye1) e2 -> dispLamOpt req x tye1 e2
    AppOptGiven e1 e2 -> dispAppOptGiven req e1 e2
    AppOptOmitted e1 -> dispAppOptOmitted req e1
    LetIn x e1 e2 -> dispLetIn req x e1 e2
    LetOpenIn m e -> dispLetOpenIn req m e
    IfThenElse e0 e1 e2 -> dispIfThenElse req e0 e1 e2
    As e1 tye2 -> dispAs req e1 tye2
    Bracket e1 -> dispBracket e1
    Escape e1 -> dispEscape e1

instance Disp (TypeExprF ann) where
  dispGen req (TypeExpr _ann typeExprMain) = dispGen req typeExprMain

instance Disp (TypeExprMainF ann) where
  dispGen req = \case
    TyName tyName args -> dispNameWithArgs req (disp tyName) (dispGen Atomic) args
    TyArrow (xOpt, tye1) tye2 -> dispArrowType req xOpt tye1 tye2
    TyCode tye1 -> dispBracket tye1
    TyOptArrow (x, tye1) tye2 -> dispOptArrowType req x tye1 tye2

instance Disp (ArgForTypeF ann) where
  dispGen req = \case
    ExprArgPersistent e -> dispPersistent e
    ExprArgNormal e -> dispGen req e
    TypeArg tye -> dispGen req tye

instance Disp BuiltIn where
  dispGen _ = \case
    BIAdd x1 x2 -> "ADD(" <> disps [x1, x2] <> ")"
    BISub x1 x2 -> "SUB(" <> disps [x1, x2] <> ")"
    BIMult x1 x2 -> "MULT(" <> disps [x1, x2] <> ")"
    BILeq x1 x2 -> "LEQ(" <> disps [x1, x2] <> ")"
    BIAssertNat _loc x1 -> "ASSERT_NAT(" <> disp x1 <> ")"
    BIListMap f x -> "LIST_MAP(" <> disps [f, x] <> ")"
    BIGenVadd x -> "GEN_VADD(" <> disp x <> ")"
    BIGenVconcat x1 x2 -> "GEN_VCONCAT(" <> disps [x1, x2] <> ")"
    BIGenMtranspose x1 x2 -> "GEN_MTRANSPOSE(" <> disps [x1, x2] <> ")"
    BIGenMmult x1 x2 x3 -> "GEN_MMULT(" <> disps [x1, x2, x3] <> ")"
    BIGenMconcatVert x1 x2 x3 -> "GEN_MCONCAT_VERT(" <> disps [x1, x2, x3] <> ")"
    BIGenTadd x -> "GEN_TADD(" <> disp x <> ")"
    BIVadd n x1 x2 -> "VADD@{" <> disp n <> "}(" <> disps [x1, x2] <> ")"
    BIVconcat m n x1 x2 -> "VCONCAT@{" <> disps [m, n] <> "}(" <> disps [x1, x2] <> ")"
    BIMtranspose m n x1 -> "MTRANSPOSE@{" <> disps [m, n] <> "}(" <> disp x1 <> ")"
    BIMmult k m n x1 x2 -> "MMULT@{" <> disps [k, m, n] <> "}(" <> disps [x1, x2] <> "}"
    BIMconcatVert m1 m2 n x1 x2 -> "MCONCAT_VERT@{" <> disps [m1, m2, n] <> "}(" <> disps [x1, x2] <> ")"
    BIDropAt x1 x2 -> "DROP_AT(" <> disps [x1, x2] <> ")"
    BIListAppend x1 x2 -> "LIST.APPEND(" <> disps [x1, x2] <> ")"
    BIListIter x1 x2 -> "LIST.ITER(" <> disps [x1, x2] <> ")"
    BIGenBroadcasted x1 x2 -> "GEN_BROADCASTED(" <> disps [x1, x2] <> ")"
    BITensorGenZeros x1 -> "TENSOR.GEN_ZEROS(" <> disp x1 <> ")"
    BITensorGenMult x1 -> "TENSOR.GEN_MULT(" <> disp x1 <> ")"
    BITensorGenGrad x1 -> "TENSOR.GEN_GRAD(" <> disp x1 <> ")"
    BITensorGenZeroGrad x1 -> "TENSOR.GEN_ZERO_GRAD(" <> disp x1 <> ")"
    BITensorGenSubUpdate x1 -> "TENSOR.GEN_SUB_UPDATE(" <> disp x1 <> ")"
    BITensorGenArgmax x1 x2 -> "TENSOR.GEN_ARGMAX(" <> disps [x1, x2] <> ")"
    BITensorGenCrossEntropyForLogits x1 x2 -> "TENSOR.GEN_CROSS_ENTROPY_FOR_LOGITS(" <> disps [x1, x2] <> ")"
    BITensorGenCountEqual x1 -> "TENSOR.GEN_COUNT_EQUAL(" <> disp x1 <> ")"
    BITadd ns x1 x2 -> "TADD@{" <> dispListLiteral ns <> "}(" <> disps [x1, x2] <> ")"

instance Disp Ass0BuiltInName where
  dispGen _ a0builtInName = "<" <> disp (show a0builtInName) <> ">"

instance Disp Ass1BuiltInName where
  dispGen _ a1builtInName = "<" <> disp (show a1builtInName) <> ">"

instance (Disp e) => Disp (Surface.Literal e) where
  dispGen _ = \case
    Surface.LitInt n -> pretty n
    Surface.LitList es -> dispListLiteral es
    Surface.LitVec ns -> dispVectorLiteral ns
    Surface.LitMat nss -> dispMatrixLiteral nss

instance Disp Surface.Expr where
  dispGen req (Surface.Expr _ann exprMain) = dispGen req exprMain

instance Disp Surface.ExprMain where
  dispGen req = \case
    Surface.Literal lit -> dispGen req lit
    Surface.Var (ms, x) -> dispLongName ms x
    Surface.Lam Nothing (x, tye1) e2 -> dispNonrecLam req x tye1 e2
    Surface.Lam (Just (f, tyeRec)) (x, tye1) e2 -> dispRecLam req f tyeRec x tye1 e2
    Surface.App e1 e2 -> dispApp req e1 e2
    Surface.LetIn x e1 e2 -> dispLetIn req x e1 e2
    Surface.IfThenElse e0 e1 e2 -> dispIfThenElse req e0 e1 e2
    Surface.As e1 tye2 -> dispAs req e1 tye2
    Surface.LamOpt (x, tye1) e2 -> dispLamOpt req x tye1 e2
    Surface.AppOptGiven e1 e2 -> dispAppOptGiven req e1 e2
    Surface.AppOptOmitted e1 -> dispAppOptOmitted req e1

instance Disp Surface.TypeExpr where
  dispGen req (Surface.TypeExpr _ann typeExprMain) = dispGen req typeExprMain

instance Disp Surface.TypeExprMain where
  dispGen req = \case
    Surface.TyName tyName args -> dispNameWithArgs req (disp tyName) (dispGen Atomic) args
    Surface.TyArrow (xOpt, tye1) tye2 -> dispArrowType req xOpt tye1 tye2
    Surface.TyOptArrow (x, tye1) tye2 -> dispOptArrowType req x tye1 tye2

instance Disp Surface.ArgForType where
  dispGen req = \case
    Surface.ExprArg e -> dispGen req e
    Surface.TypeArg tye -> dispGen req tye

instance (Disp e) => Disp (AssLiteral e) where
  dispGen _ = \case
    ALitInt n -> pretty n
    ALitFloat r -> pretty r
    ALitBool True -> "true"
    ALitBool False -> "false"
    ALitUnit -> "()"
    ALitList es -> dispListLiteral es
    ALitVec v -> dispVectorLiteral (Vector.toList v)
    ALitMat m -> dispMatrixLiteral (Matrix.toRows m)

instance Disp Ass0Expr where
  dispGen req = \case
    A0Literal lit -> disp lit
    A0AppBuiltIn bi -> disp bi
    A0Var y -> disp y
    A0BuiltInName builtInName -> disp builtInName
    A0Lam Nothing (y, a0tye1) a0e2 -> dispNonrecLam req y a0tye1 a0e2
    A0Lam (Just (f, a0tyeRec)) (y, a0tye1) a0e2 -> dispRecLam req f a0tyeRec y a0tye1 a0e2
    A0App a0e1 a0e2 -> dispApp req a0e1 a0e2
    A0LetIn (y, a0tye1) a0e1 a0e2 -> dispLetInWithAnnot req y a0tye1 a0e1 a0e2
    A0Bracket a1e1 -> dispBracket a1e1
    A0IfThenElse a0e0 a0e1 a0e2 -> dispIfThenElse req a0e0 a0e1 a0e2
    A0TyEqAssert _loc ty1eq ->
      let (a1tye1, a1tye2) = decomposeType1Equation ty1eq
       in group (assertionStyle ("{" <> dispBracket a1tye1 <+> "=>" <+> dispBracket a1tye2 <> "}"))

instance Disp Ass1Expr where
  dispGen req = \case
    A1Literal lit -> disp lit
    A1Var x -> disp x
    A1BuiltInName a1builtInName -> disp a1builtInName
    A1Lam Nothing (x, a1tye1) a1e2 -> dispNonrecLam req x a1tye1 a1e2
    A1Lam (Just (f, a1tyeRec)) (x, a1tye1) a1e2 -> dispRecLam req f a1tyeRec x a1tye1 a1e2
    A1App a1e1 a1e2 -> dispApp req a1e1 a1e2
    A1IfThenElse a1e0 a1e1 a1e2 -> dispIfThenElse req a1e0 a1e1 a1e2
    A1Escape a0e1 -> dispEscape a0e1

instance Disp Ass0PrimType where
  dispGen req = \case
    A0TyInt -> "Int"
    A0TyNat -> "Nat"
    A0TyFloat -> "Float"
    A0TyBool -> "Bool"
    A0TyUnit -> "Unit"
    A0TyTensor [n] -> dispNameWithArgs req "Vec" disp [n]
    A0TyTensor [m, n] -> dispNameWithArgs req "Mat" disp [m, n]
    A0TyTensor ns -> dispNameWithArgs req "Tensor" dispListLiteral [ns]

instance Disp Ass0TypeExpr where
  dispGen req = \case
    A0TyPrim a0tyPrim -> disp a0tyPrim
    A0TyList a0tye -> dispListType req a0tye
    A0TyArrow (xOpt, a0tye1) a0tye2 -> dispArrowType req xOpt a0tye1 a0tye2
    A0TyCode a1tye1 -> dispBracket a1tye1
    A0TyOptArrow (x, a0tye1) a0tye2 -> dispOptArrowType req x a0tye1 a0tye2

instance Disp StrictAss0TypeExpr where
  dispGen req = \case
    SA0TyPrim a0tyPrim -> disp a0tyPrim
    SA0TyList sa0tye -> dispListType req sa0tye
    SA0TyArrow (xOpt, sa0tye1) sa0tye2 -> dispArrowType req xOpt sa0tye1 sa0tye2
    SA0TyCode a1tye1 -> dispBracket a1tye1

instance Disp Ass1PrimType where
  dispGen req = \case
    A1TyInt -> "Int"
    A1TyFloat -> "Float"
    A1TyBool -> "Bool"
    A1TyUnit -> "Unit"
    A1TyTensor a0eList ->
      case a0eList of
        A0Literal (ALitList [a0e]) -> dispNameWithArgs req "Vec" dispPersistent [a0e]
        A0Literal (ALitList [a0e1, a0e2]) -> dispNameWithArgs req "Mat" dispPersistent [a0e1, a0e2]
        _ -> dispNameWithArgs req "Tensor" dispPersistent [a0eList]

instance Disp Ass1TypeExpr where
  dispGen req = \case
    A1TyPrim a1tyPrim -> dispGen req a1tyPrim
    A1TyList a1tye -> dispListType req a1tye
    A1TyArrow a1tye1 a1tye2 -> dispNondepArrowType req a1tye1 a1tye2

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
    UnboundVar spanInFile ms x ->
      "Unbound variable" <+> dispLongName ms x <+> disp spanInFile
    UnboundModule spanInFile m ->
      "Unbound module" <+> disp m <+> disp spanInFile
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
    NotAnIntListLitArgAtStage0 spanInFile a0e ->
      "An argument expression at stage 0 is not an integer list literal:" <+> stage0Style (disp a0e) <+> disp spanInFile
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
    NotABoolTypeForStage0 spanInFile a0tye ->
      "Not bool (at stage 0):" <+> stage1Style (disp a0tye) <+> disp spanInFile
    NotABoolTypeForStage1 spanInFile a1tye ->
      "Not bool (at stage 1):" <+> stage1Style (disp a1tye) <+> disp spanInFile
    NotACodeType spanInFile a0tye ->
      "Not a code type:" <+> stage0Style (disp a0tye) <+> disp spanInFile
    CannotUseEscapeAtStage0 spanInFile ->
      "Cannot use Escape (~) at stage 0" <+> disp spanInFile
    CannotUseBracketAtStage1 spanInFile ->
      "Cannot use Bracket (&) at stage 1" <+> disp spanInFile
    CannotUseLamOptAtStage1 spanInFile ->
      "Cannot use optional function (fun{...} ->) at stage 1" <+> disp spanInFile
    CannotUseAppOptGivenAtStage1 spanInFile ->
      "Cannot use optional application (... {...}) at stage 1" <+> disp spanInFile
    CannotUseAppOptOmittedAtStage1 spanInFile ->
      "Cannot use optional application (... _) at stage 1" <+> disp spanInFile
    FunctionTypeCannotBeDependentAtStage1 spanInFile x ->
      "Function types cannot be dependent at stage 1:" <+> disp x <+> disp spanInFile
    CannotUseCodeTypeAtStage1 spanInFile ->
      "Cannot use code types at stage 1" <+> disp spanInFile
    CannotUseOptArrowTypeAtStage1 spanInFile ->
      "Cannot use optional function types at stage 1" <+> disp spanInFile
    CannotUsePersistentArgAtStage0 spanInFile ->
      "Cannot use persistent arguments at stage 0" <+> disp spanInFile
    CannotUseNormalArgAtStage1 spanInFile ->
      "Cannot use normal arguments at stage 1" <+> disp spanInFile
    VarOccursFreelyInAss0Type spanInFile x a0result ->
      "Variable" <+> disp x <+> "occurs in stage-0 type" <+> stage0Style (disp a0result) <+> disp spanInFile
    VarOccursFreelyInAss1Type spanInFile x a1result ->
      "Variable" <+> disp x <+> "occurs in stage-1 type" <+> stage1Style (disp a1result) <+> disp spanInFile
    InvalidMatrixLiteral spanInFile e ->
      "Invalid matrix literal;" <+> disp e <+> disp spanInFile
    CannotMergeTypesByConditional0 spanInFile a0tye1 a0tye2 condErr ->
      "Cannot merge stage-0 types by conditionals"
        <+> disp spanInFile
        <> hardline
        <+> "left:"
        <> nest 2 (hardline <> stage0Style (disp a0tye1))
        <> hardline
        <+> "right:"
        <> nest 2 (hardline <> stage0Style (disp a0tye2))
        <> hardline
        <> disp condErr
    CannotMergeTypesByConditional1 spanInFile a1tye1 a1tye2 condErr ->
      "Cannot merge stage-1 types by conditionals"
        <+> disp spanInFile
        <> hardline
        <+> "left:"
        <> nest 2 (hardline <> stage1Style (disp a1tye1))
        <> hardline
        <+> "right:"
        <> nest 2 (hardline <> stage1Style (disp a1tye2))
        <> hardline
        <> disp condErr
    CannotMergeResultsByConditionals spanInFile result1 result2 ->
      "Cannot merge results by conditionals"
        <+> disp spanInFile
        <> hardline
        <+> "left:"
        <> nest 2 (hardline <> disp result1)
        <> hardline
        <+> "right:"
        <> nest 2 (hardline <> disp result2)
    CannotApplyLiteral spanInFile ->
      "Cannot apply a literal" <> disp spanInFile
    CannotInstantiateGuidedByAppContext0 spanInFile appCtx a0tye ->
      "Cannot instantiate a stage-0 type guided by the application context"
        <+> disp spanInFile
        <> hardline
        <+> "application context:"
        <> nest 2 (hardline <> disps appCtx)
        <> hardline
        <+> "type:"
        <> nest 2 (hardline <> stage0Style (disp a0tye))
    CannotInstantiateGuidedByAppContext1 spanInFile appCtx a1tye ->
      "Cannot instantiate a stage-1 type guided by the application context"
        <+> disp spanInFile
        <> hardline
        <+> "application context:"
        <> nest 2 (hardline <> disps appCtx)
        <> hardline
        <+> "type:"
        <> nest 2 (hardline <> stage1Style (disp a1tye))
    CannotInferOptional spanInFile x ->
      "Cannot infer an optional argument for" <+> disp x <+> disp spanInFile
    Stage1IfThenElseRestrictedToEmptyContext spanInFile appCtx ->
      "Stage-1 if-expressions are restricted to be used at empty application contexts"
        <+> disp spanInFile
        <> hardline
        <+> "application context:"
        <> nest 2 (hardline <> disps appCtx)
    BindingOverwritten spanInFile x ->
      "value " <+> disp x <+> "is overwritten by another binding" <+> disp spanInFile
    UnknownExternalName spanInFile extName ->
      "Unknown external name" <+> disp extName <+> disp spanInFile
    InvalidPersistentType spanInFile a0tye ->
      "Invalid persistent type:" <+> stage0Style (disp a0tye) <+> disp spanInFile

instance Disp ConditionalMergeError where
  dispGen _ = \case
    CannotMerge0 a0tye1 a0tye2 ->
      "types" <+> stage0Style (disp a0tye1) <+> "and" <+> stage0Style (disp a0tye2) <+> "are incompatible"
    CannotMerge1 a1tye1 a1tye2 ->
      "types" <+> stage1Style (disp a1tye1) <+> "and" <+> stage1Style (disp a1tye2) <+> "are incompatible"

instance Disp AppContextEntry where
  dispGen _ = \case
    AppArg0 a0e a0tye -> stage0Style (disp a0e) <+> ":" <+> stage0Style (disp a0tye)
    AppArg1 a1tye -> stage1Style (disp a1tye)
    AppArgOptGiven0 a0e a0tye -> "{" <> stage0Style (disp a0e) <+> ":" <+> stage0Style (disp a0tye) <> "}"
    AppArgOptOmitted0 -> "_"

instance (Disp a) => Disp (Result a) where
  dispGen _ = \case
    Pure v -> disp v -- TODO (enhance): add `stage0Style` etc.
    Cast0 _ a0tye r -> "cast0 :" <+> stage0Style (disp a0tye) <> ";" <+> disp r
    Cast1 _ a1tye r -> "cast1 :" <+> stage1Style (disp a1tye) <> ";" <+> disp r
    CastGiven0 _ a0tye r -> "cast-given0 :" <+> stage0Style (disp a0tye) <> ";" <+> disp r
    FillInferred0 a0e r -> "fill0" <+> disp a0e <> ";" <+> disp r
    InsertInferred0 a0e r -> "insert0" <+> disp a0e <> ";" <+> disp r

instance Disp Ass0Val where
  dispGen req = \case
    A0ValLiteral lit -> disp lit
    A0ValLam Nothing (x, a0tyv1) a0v2 _env -> dispNonrecLam req x a0tyv1 a0v2
    A0ValLam (Just (f, a0tyvRec)) (x, a0tyv1) a0v2 _env -> dispRecLam req f a0tyvRec x a0tyv1 a0v2
    A0ValBracket a1v1 -> dispBracket a1v1

instance Disp Ass1ValConst where
  dispGen _ = \case
    A1ValConstVadd n -> "vadd@{" <> disp n <> "}"
    A1ValConstVconcat m n -> "vconcat@{" <> disps [m, n] <> "}"
    A1ValConstMtranspose m n -> "mtranspose@{" <> disps [m, n] <> "}"
    A1ValConstMmult k m n -> "mmult@{" <> disps [k, m, n] <> "}"
    A1ValConstMconcatVert m1 m2 n -> "mconcat_vert@{" <> disps [m1, m2, n] <> "}"
    A1ValConstBroadcasted ns1 ns2 -> "broadcasted@{" <> dispListLiteral ns1 <> "," <+> dispListLiteral ns2 <> "}"
    A1ValConstTensorZeros ns1 -> "Tensor.zeros@{" <> dispListLiteral ns1 <> "}"
    A1ValConstTensorMult ns1 -> "Tensor.mult@{" <> dispListLiteral ns1 <> "}"
    A1ValConstTensorGrad ns1 -> "Tensor.grad@{" <> dispListLiteral ns1 <> "}"
    A1ValConstTensorZeroGrad ns1 -> "Tensor.zero_grad@{" <> dispListLiteral ns1 <> "}"
    A1ValConstTensorSubUpdate ns1 -> "Tensor.sub_update@{" <> dispListLiteral ns1 <> "}"
    A1ValConstTensorArgmax ns1 n2 -> "Tensor.argmax@{" <> dispListLiteral ns1 <> "," <+> disp n2 <> "}"
    A1ValConstTensorCrossEntropyForLogits n1 n2 -> "Tensor.cross_entropy_for_logits@{" <> disps [n1, n2] <> "}"
    A1ValConstTensorCountEqual ns -> "Tensor.count_equal@{" <> dispListLiteral ns <> "}"
    A1ValConstTadd ns -> "Tensor.tadd@{" <> dispListLiteral ns <> "}"
    A1ValConstBuiltInName a1builtInName -> disp a1builtInName

instance Disp Ass1Val where
  dispGen req = \case
    A1ValLiteral lit -> disp lit
    A1ValConst c -> disp c
    A1ValVar symb -> disp symb
    A1ValLam Nothing (symbX, a1tyv1) a1v2 ->
      dispNonrecLam req symbX a1tyv1 a1v2
    A1ValLam (Just (symbF, a1tyvRec)) (symbX, a1tyv1) a1v2 ->
      dispRecLam req symbF a1tyvRec symbX a1tyv1 a1v2
    A1ValApp a1v1 a1v2 ->
      dispApp req a1v1 a1v2
    A1ValIfThenElse a1v0 a1v1 a1v2 ->
      dispIfThenElse req a1v0 a1v1 a1v2

instance Disp Ass0TypeVal where
  dispGen req = \case
    A0TyValPrim a0tyvPrim -> dispGen req a0tyvPrim
    A0TyValList a0tyv1 -> dispListType req a0tyv1
    A0TyValArrow (xOpt, a0tyv1) a0tye2 -> dispArrowType req xOpt a0tyv1 a0tye2
    A0TyValCode a1tyv1 -> dispBracket a1tyv1

instance Disp Ass0PrimTypeVal where
  dispGen req = \case
    A0TyValInt -> "Int"
    A0TyValNat -> "Nat"
    A0TyValFloat -> "Float"
    A0TyValBool -> "Bool"
    A0TyValUnit -> "Unit"
    A0TyValTensor [n] -> dispNameWithArgs req "Vec" disp [n]
    A0TyValTensor [m, n] -> dispNameWithArgs req "Mat" disp [m, n]
    A0TyValTensor ns -> dispNameWithArgs req "Tensor" dispListLiteral [ns]

instance Disp Ass1TypeVal where
  dispGen req = \case
    A1TyValPrim a1tyvPrim -> dispGen req a1tyvPrim
    A1TyValList a1tyv -> dispListType req a1tyv
    A1TyValArrow a1tyv1 a1tyv2 -> dispNondepArrowType req a1tyv1 a1tyv2

instance Disp Ass1PrimTypeVal where
  dispGen req = \case
    A1TyValInt -> "Int"
    A1TyValFloat -> "Float"
    A1TyValBool -> "Bool"
    A1TyValUnit -> "Unit"
    A1TyValTensor [n] -> dispNameWithArgs req "Vec" dispPersistent [n]
    A1TyValTensor [m, n] -> dispNameWithArgs req "Mat" dispPersistent [m, n]
    A1TyValTensor ns -> dispNameWithArgs req "Tensor" dispPersistentListLiteral [ns]

instance Disp LocationInFile where
  dispGen _ (LocationInFile l c) =
    "line" <+> disp l <> ", column" <+> disp (c - 1)

instance Disp SpanInFile where
  dispGen _ (SpanInFile {startLocation, endLocation, contents}) =
    "(from" <+> disp startLocation <+> "to" <+> disp endLocation <> ")" <> maybe mempty makeLineText contents
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
    Evaluator.NotAList Nothing a0v ->
      "Not a list:" <+> disp a0v
    Evaluator.NotAList (Just x) a0v ->
      "Not a list:" <+> disp a0v <+> "(bound to:" <+> disp x <> ")"
    Evaluator.NotAVector x a0v ->
      "Not a vector:" <+> disp a0v <+> "(bound to:" <+> disp x <> ")"
    Evaluator.NotAMatrix x a0v ->
      "Not a matrix:" <+> disp a0v <+> "(bound to:" <+> disp x <> ")"
    Evaluator.NotABoolean a0v ->
      "Not a Boolean:" <+> disp a0v
    Evaluator.NotAUnit a0v ->
      "Not a unit:" <+> disp a0v
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
        <> "got:"
        <> nest 2 (hardline <> disp a1tyv1)
        <> hardline
        <> "expected:"
        <> nest 2 (hardline <> disp a1tyv2)
    Evaluator.NatAssertionFailure spanInFile n ->
      "Assertion failure of downcasting Int to Nat"
        <+> disp spanInFile
        <> hardline
        <> "got:"
        <+> disp n

instance Disp Bta.AnalysisError where
  dispGen _ = \case
    Bta.UnboundVar spanInFile x ->
      "Unbound variable" <+> disp x <+> disp spanInFile
    Bta.NotAFunction spanInFile bity ->
      "Not a function type;" <+> disp (show bity) <+> disp spanInFile
    Bta.NotAnOptFunction spanInFile bity ->
      "Not an optional function type;" <+> disp (show bity) <+> disp spanInFile
    Bta.NotABase spanInFile bity ->
      "Not a base type;" <+> disp (show bity) <+> disp spanInFile
    Bta.BindingTimeContradiction spanInFile ->
      "Binding-time contradiction" <+> disp spanInFile
    Bta.BITypeContradiction spanInFile bity1 bity2 ->
      "Basic type contradiction;" <+> disp (show bity1) <> "," <+> disp (show bity2) <+> disp spanInFile
    Bta.UnknownTypeOrInvalidArgs spanInFile _tyName _args ->
      -- TODO (enhance): detailed report
      "Unknown type or invalid arguments" <+> disp spanInFile

dispWithBindingTime :: (Disp exprMain) => Bta.BindingTimeConst -> exprMain -> Doc Ann
dispWithBindingTime btc eMain =
  group (f (prefix <> "(") <> disp eMain <> f ")")
  where
    (f, prefix) =
      case btc of
        Bta.BT0 -> (bindingTime0Style, "$0")
        Bta.BT1 -> (bindingTime1Style, "$1")

instance Disp (Bta.BCExprF ann) where
  dispGen _ (Surface.Expr (btc, _ann) exprMain) =
    dispWithBindingTime btc exprMain

instance Disp (Bta.BCExprMainF ann) where
  dispGen req = \case
    Surface.Literal lit -> disp lit
    Surface.Var (ms, x) -> dispLongName ms x
    Surface.Lam Nothing (x, tye1) e2 -> dispNonrecLam req x tye1 e2
    Surface.Lam (Just (f, tyeRec)) (x, tye1) e2 -> dispRecLam req f tyeRec x tye1 e2
    Surface.App e1 e2 -> dispApp req e1 e2
    Surface.LetIn x e1 e2 -> dispLetIn req x e1 e2
    Surface.IfThenElse e0 e1 e2 -> dispIfThenElse req e0 e1 e2
    Surface.As e1 tye2 -> dispAs req e1 tye2
    Surface.LamOpt (x, tye1) e2 -> dispLamOpt req x tye1 e2
    Surface.AppOptGiven e1 e2 -> dispAppOptGiven req e1 e2
    Surface.AppOptOmitted e1 -> dispAppOptOmitted req e1

instance Disp (Bta.BCTypeExprF ann) where
  dispGen _ (Surface.TypeExpr (btc, _ann) typeExprMain) =
    dispWithBindingTime btc typeExprMain

instance Disp (Bta.BCTypeExprMainF ann) where
  dispGen req = \case
    Surface.TyName tyName args -> dispNameWithArgs req (disp tyName) (dispGen Atomic) args
    Surface.TyArrow (xOpt, tye1) tye2 -> dispArrowType req xOpt tye1 tye2
    Surface.TyOptArrow (x, tye1) tye2 -> dispOptArrowType req x tye1 tye2

instance Disp (Bta.BCArgForTypeF ann) where
  dispGen req = \case
    Surface.ExprArg e -> dispGen req e
    Surface.TypeArg tye -> dispGen req tye
