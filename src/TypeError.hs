module TypeError
  ( TypeError (..),
  )
where

import Syntax

data TypeError
  = UnboundVar Var
  | UnknownTypeOrInvalidArity TypeName Int
  | TypeContradiction TypeExpr TypeExpr
  | NotAFunctionTypeForStage0 TypeExpr
  | NotAFunctionTypeForStage1 TypeExpr
  | NotACodeType TypeExpr
  | CannotUseEscapeAtStage0
  | CannotUseBracketAtStage1
  | FunctionTypeCannotBeDependentAtStage1 Var
  | CannotUseCodeTypeAtStage1
  deriving stock (Show)
