module Lwsd.SrcSyntax
  ( Var,
    Literal (..),
    ExprF (..),
    ExprMainF (..),
    LamBinderF (..),
    Expr,
    ExprMain,
    LamBinder,
    TypeName,
    TypeVar (..),
    TypeExprF (..),
    TypeExprMainF (..),
    TypeExpr,
    TypeExprMain,
    ArgForTypeF (..),
    ArgForType,
    BindF (..),
    BindMainF (..),
    Bind,
    BindValF (..),
    BindVal,
    Stage (..),
    ExternalField,
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
  | LitFloat Double
  | LitUnit
  | LitString Text
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
  | Var ([Var], Var) -- A module name chain and a value identifier
  | Lam (Maybe (Var, TypeExprF ann)) (Var, TypeExprF ann) (ExprF ann)
  | App (ExprF ann) (ExprF ann)
  | LetIn Var [LamBinderF ann] (ExprF ann) (ExprF ann)
  | LetRecIn Var [LamBinderF ann] (TypeExprF ann) (ExprF ann) (ExprF ann)
  | LetTupleIn Var Var (ExprF ann) (ExprF ann)
  | IfThenElse (ExprF ann) (ExprF ann) (ExprF ann)
  | As (ExprF ann) (TypeExprF ann)
  | Bracket (ExprF ann)
  | Escape (ExprF ann)
  | LamOpt (Var, TypeExprF ann) (ExprF ann)
  | AppOptGiven (ExprF ann) (ExprF ann)
  | AppOptOmitted (ExprF ann)
  | LetOpenIn Var (ExprF ann)
  | Sequential (ExprF ann) (ExprF ann)
  | Tuple (ExprF ann) (ExprF ann) -- TODO: generalize tuples
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 ExprMainF)

data LamBinderF ann
  = MandatoryBinder (Var, TypeExprF ann)
  | OptionalBinder (Var, TypeExprF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 LamBinderF)

-- The type for ASTs for expressions obtained by parsing source programs.
type Expr = ExprF Span

type ExprMain = ExprMainF Span

type LamBinder = LamBinderF Span

type TypeName = Text

newtype TypeVar = TypeVar Text
  deriving stock (Eq, Show)

data TypeExprF ann = TypeExpr ann (TypeExprMainF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 TypeExprF)

data TypeExprMainF ann
  = TyName TypeName [ArgForTypeF ann]
  | TyVar TypeVar
  | TyArrow (Maybe Var, TypeExprF ann) (TypeExprF ann)
  | TyCode (TypeExprF ann)
  | TyOptArrow (Var, TypeExprF ann) (TypeExprF ann)
  | TyRefinement Var (TypeExprF ann) (ExprF ann)
  | TyProduct (TypeExprF ann) (TypeExprF ann)
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

data BindF ann = Bind ann (BindMainF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 BindF)

data BindMainF ann
  = BindVal Stage Var (BindValF ann)
  | BindModule Var [BindF ann]
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 BindMainF)

type Bind = BindF Span

data BindValF ann
  = BindValExternal [TypeVar] (TypeExprF ann) External
  | BindValNormal (ExprF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Eq1, Show1) via (Generically1 BindValF)

type BindVal = BindValF Span

data Stage = Stage0 | Stage1 | StagePers
  deriving stock (Eq, Show)

type ExternalField = Text

type External = [(ExternalField, Text)]
