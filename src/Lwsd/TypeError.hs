module Lwsd.TypeError
  ( TypeError (..),
  )
where

import Lwsd.Syntax
import Util.LocationInFile (SpanInFile)
import Util.Matrix qualified as Matrix
import Prelude

data TypeError
  = UnboundVar SpanInFile Var
  | NotAStage0Var SpanInFile Var
  | NotAStage1Var SpanInFile Var
  | UnknownTypeOrInvalidArityAtStage0 SpanInFile TypeName Int
  | UnknownTypeOrInvalidArityAtStage1 SpanInFile TypeName Int
  | NotAnIntLitArgAtStage0 SpanInFile Ass0Expr
  | NotAnIntTypedArgAtStage1 SpanInFile Ass0TypeExpr
  | TypeContradictionAtStage0 SpanInFile Ass0TypeExpr Ass0TypeExpr
  | TypeContradictionAtStage1 SpanInFile Ass1TypeExpr Ass1TypeExpr
  | NotAFunctionTypeForStage0 SpanInFile Ass0TypeExpr
  | NotAFunctionTypeForStage1 SpanInFile Ass1TypeExpr
  | NotACodeType SpanInFile Ass0TypeExpr
  | CannotUseEscapeAtStage0 SpanInFile
  | CannotUseBracketAtStage1 SpanInFile
  | FunctionTypeCannotBeDependentAtStage1 SpanInFile Var
  | CannotUseCodeTypeAtStage1 SpanInFile
  | CannotUsePersistentArgAtStage0 SpanInFile
  | CannotUseNormalArgAtStage1 SpanInFile
  | VarOccursFreelyInAss0Type SpanInFile Var Ass0TypeExpr
  | VarOccursFreelyInAss1Type SpanInFile Var Ass1TypeExpr
  | InvalidMatrixLiteral SpanInFile Matrix.ConstructionError
  deriving stock (Eq, Show)
