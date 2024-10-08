module Lwsd.TypeError
  ( TypeError (..),
  )
where

import Lwsd.Matrix qualified as Matrix
import Lwsd.Syntax
import Prelude

data TypeError
  = UnboundVar Var
  | NotAStage0Var Var
  | NotAStage1Var Var
  | UnknownTypeOrInvalidArityAtStage0 TypeName Int
  | UnknownTypeOrInvalidArityAtStage1 TypeName Int
  | NotAnIntLitArgAtStage0 Ass0Expr
  | NotAnIntTypedArgAtStage1 Ass0TypeExpr
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
  | VarOccursFreelyInAss0Type Var Ass0TypeExpr
  | VarOccursFreelyInAss1Type Var Ass1TypeExpr
  | InvalidMatrixLiteral Matrix.ConstructionError
  deriving stock (Eq, Show)
