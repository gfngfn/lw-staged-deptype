module Lwsd.SrcSyntax
  ( Var,
    Literal (..),
    ExprF (..),
    ExprMainF (..),
    Expr,
    ExprMain,
    TypeName,
    TypeExprF (..),
    TypeExprMainF (..),
    TypeExpr,
    TypeExprMain,
    ArgForTypeF (..),
    ArgForType,
    Decl,
    DeclF (..),
    DeclMainF (..),
    External,
  )
where

import Data.Functor.Classes
import Data.Text (Text)
import Generic.Data
import Generic.Data.Orphans ()
import Util.TokenUtil (Span)
import Prelude

type Var = Text

data Literal e
  = LitInt Int
  | LitList [e]
  | LitVec [Int]
  | LitMat [[Int]]
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 Literal)

data ExprF ann = Expr ann (ExprMainF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 ExprF)

data ExprMainF ann
  = Literal (Literal (ExprF ann))
  | Var Var
  | Lam (Maybe (Var, TypeExprF ann)) (Var, TypeExprF ann) (ExprF ann)
  | App (ExprF ann) (ExprF ann)
  | LetIn Var (ExprF ann) (ExprF ann)
  | IfThenElse (ExprF ann) (ExprF ann) (ExprF ann)
  | As (ExprF ann) (TypeExprF ann)
  | Bracket (ExprF ann)
  | Escape (ExprF ann)
  | LamOpt (Var, TypeExprF ann) (ExprF ann)
  | AppOptGiven (ExprF ann) (ExprF ann)
  | AppOptOmitted (ExprF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 ExprMainF)

-- The type for ASTs for expressions obtained by parsing source programs.
type Expr = ExprF Span

type ExprMain = ExprMainF Span

type TypeName = Text

data TypeExprF ann = TypeExpr ann (TypeExprMainF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 TypeExprF)

data TypeExprMainF ann
  = TyName TypeName [ArgForTypeF ann]
  | TyArrow (Maybe Var, TypeExprF ann) (TypeExprF ann)
  | TyCode (TypeExprF ann)
  | TyOptArrow (Var, TypeExprF ann) (TypeExprF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 TypeExprMainF)

-- The type for ASTs for type expressions obtained by parsing source programs.
type TypeExpr = TypeExprF Span

type TypeExprMain = TypeExprMainF Span

data ArgForTypeF ann
  = ExprArgPersistent (ExprF ann)
  | ExprArgNormal (ExprF ann)
  | TypeArg (TypeExprF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 ArgForTypeF)

type ArgForType = ArgForTypeF Span

type Decl = DeclF Span

data DeclF ann = Decl ann (DeclMainF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 DeclF)

data DeclMainF ann
  = DeclVal0 Var (TypeExprF ann) External Text
  | DeclVal1 Var (TypeExprF ann) External Text
  | DeclValPers Var (TypeExprF ann) External Text
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 DeclMainF)

type External = Text
