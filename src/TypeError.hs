module TypeError
  ( TypeError (..),
  )
where

import Syntax

data TypeError
  = UnboundVar Var
  | NotAStage0Var Var
  | NotAStage1Var Var
  | UnknownTypeOrInvalidArity TypeName Int
  | TypeContradictionAtStage0 Ass0TypeExpr Ass0TypeExpr
  | TypeContradictionAtStage1 Ass1TypeExpr Ass1TypeExpr
  | NotAFunctionTypeForStage0 Ass0TypeExpr
  | NotAFunctionTypeForStage1 Ass1TypeExpr
  | NotACodeType Ass0TypeExpr
  | CannotUseEscapeAtStage0
  | CannotUseBracketAtStage1
  | FunctionTypeCannotBeDependentAtStage1 Var
  | CannotUseCodeTypeAtStage1
  | CannotUsePersistentArgAtStage0
  | CannotUseNormalArgAtStage1
  deriving stock (Show)
