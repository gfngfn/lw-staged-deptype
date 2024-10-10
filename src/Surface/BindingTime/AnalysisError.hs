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
  | BindingTimeContradiction SpanInFile
  deriving stock (Show)
