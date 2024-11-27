module Surface.BindingTime.AnalysisError
  ( AnalysisError (..),
  )
where

import Surface.BindingTime.Core
import Surface.Syntax
import Util.LocationInFile (SpanInFile)
import Prelude

data AnalysisError
  = UnboundVar SpanInFile Var
  | NotAFunction SpanInFile BIType
  | NotAnOptFunction SpanInFile BIType
  | NotABase SpanInFile BIType
  | BindingTimeContradiction SpanInFile
  | BITypeContradiction SpanInFile BIType BIType
  | UnknownTypeOrInvalidArgs SpanInFile TypeName [BArgForType]
  deriving stock (Show)
