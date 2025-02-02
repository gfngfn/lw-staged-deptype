module Lwsd.EvalError
  ( Bug (..),
    EvalError (..),
  )
where

import Lwsd.BuiltIn.Core
import Lwsd.Syntax
import Util.LocationInFile (SpanInFile)
import Prelude

data Bug
  = UnboundVarFound AssVar
  | NotAClosure Ass0Val
  | NotACodeValue Ass0Val
  | NotAnInteger Ass0Val
  | NotAList Ass0Val
  | NotAVector Ass0Val
  | NotAMatrix Ass0Val
  | NotABoolean Ass0Val
  | NotAUnit Ass0Val
  | FoundSymbol AssVar Symbol
  | FoundAss0Val AssVar Ass0Val
  | InconsistentAppBuiltInArity1 BuiltInArity1 Ass0Val
  | InconsistentAppBuiltInArity2 BuiltInArity2 Ass0Val Ass0Val
  | BroadcastFailed [Int] [Int]
  deriving stock (Eq, Show)

data EvalError
  = Bug Bug
  | AssertionFailure SpanInFile Ass1TypeVal Ass1TypeVal
  | RefinementAssertionFailure SpanInFile Ass0Val
