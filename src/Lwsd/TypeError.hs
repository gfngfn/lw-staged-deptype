module Lwsd.TypeError
  ( TypeError (..),
    ConditionalMergeError (..),
  )
where

import Data.Text (Text)
import Lwsd.SrcSyntax
import Lwsd.Syntax
import Util.LocationInFile (SpanInFile)
import Util.Matrix qualified as Matrix
import Prelude

data TypeError
  = UnboundVar SpanInFile [Var] Var
  | UnboundModule SpanInFile Var
  | NotAStage0Var SpanInFile Var
  | NotAStage1Var SpanInFile Var
  | UnknownTypeOrInvalidArityAtStage0 SpanInFile TypeName Int
  | UnknownTypeOrInvalidArityAtStage1 SpanInFile TypeName Int
  | NotAnIntLitArgAtStage0 SpanInFile Ass0Expr
  | NotAnIntListLitArgAtStage0 SpanInFile Ass0Expr
  | TypeContradictionAtStage0 SpanInFile Ass0TypeExpr Ass0TypeExpr
  | TypeContradictionAtStage1 SpanInFile Ass1TypeExpr Ass1TypeExpr
  | NotABoolTypeForStage0 SpanInFile Ass0TypeExpr
  | NotABoolTypeForStage1 SpanInFile Ass1TypeExpr
  | NotAUnitTypeForStage0 SpanInFile Ass0TypeExpr
  | NotAUnitTypeForStage1 SpanInFile Ass1TypeExpr
  | NotACodeType SpanInFile Ass0TypeExpr
  | CannotUseEscapeAtStage0 SpanInFile
  | CannotUseBracketAtStage1 SpanInFile
  | CannotUseLamOptAtStage1 SpanInFile
  | CannotUseAppOptGivenAtStage1 SpanInFile
  | CannotUseAppOptOmittedAtStage1 SpanInFile
  | FunctionTypeCannotBeDependentAtStage1 SpanInFile Var
  | CannotUseCodeTypeAtStage1 SpanInFile
  | CannotUseOptArrowTypeAtStage1 SpanInFile
  | CannotUseRefinementTypeAtStage1 SpanInFile
  | CannotUsePersistentArgAtStage0 SpanInFile
  | CannotUseNormalArgAtStage1 SpanInFile
  | VarOccursFreelyInAss0Type SpanInFile Var (Result Ass0TypeExpr)
  | VarOccursFreelyInAss1Type SpanInFile Var (Result Ass1TypeExpr)
  | InvalidMatrixLiteral SpanInFile Matrix.ConstructionError
  | CannotMergeTypesByConditional0 SpanInFile Ass0TypeExpr Ass0TypeExpr ConditionalMergeError
  | CannotMergeTypesByConditional1 SpanInFile Ass1TypeExpr Ass1TypeExpr ConditionalMergeError
  | CannotMergeResultsByConditionals SpanInFile (Result Ass0TypeExpr) (Result Ass0TypeExpr)
  | CannotApplyLiteral SpanInFile
  | CannotInstantiateGuidedByAppContext0 SpanInFile AppContext Ass0TypeExpr
  | CannotInstantiateGuidedByAppContext1 SpanInFile AppContext Ass1TypeExpr
  | CannotInferOptional SpanInFile AssVar
  | Stage1IfThenElseRestrictedToEmptyContext SpanInFile AppContext
  | BindingOverwritten SpanInFile Var
  | UnknownExternalName SpanInFile Text
  | InvalidPersistentType SpanInFile Ass0TypeExpr
  | InvalidTypeForRefinement SpanInFile Ass0TypeExpr
  | NoBuiltInNameInExternal SpanInFile
  deriving stock (Eq, Show)

data ConditionalMergeError
  = CannotMerge0 Ass0TypeExpr Ass0TypeExpr
  | CannotMerge1 Ass1TypeExpr Ass1TypeExpr
  deriving stock (Eq, Show)
