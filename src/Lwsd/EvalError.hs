module Lwsd.EvalError
  ( BugF (..),
    EvalErrorF (..),
    Bug,
    EvalError,
  )
where

import Lwsd.BuiltIn.Core
import Lwsd.Syntax
import Util.LocationInFile (SpanInFile)
import Prelude

data BugF sv
  = UnboundVarFound (AssVarF sv)
  | NotAClosure (Ass0ValF sv)
  | NotACodeValue (Ass0ValF sv)
  | NotAnInteger (Ass0ValF sv)
  | NotAList (Ass0ValF sv)
  | NotAVector (Ass0ValF sv)
  | NotAMatrix (Ass0ValF sv)
  | NotABoolean (Ass0ValF sv)
  | NotAUnit (Ass0ValF sv)
  | NotAString (Ass0ValF sv)
  | NotATuple (Ass0ValF sv)
  | FoundSymbol (AssVarF sv) Symbol
  | FoundAss0Val (AssVarF sv) (Ass0ValF sv)
  | InconsistentAppBuiltInArity1 BuiltInArity1 (Ass0ValF sv)
  | InconsistentAppBuiltInArity2 BuiltInArity2 (Ass0ValF sv) (Ass0ValF sv)
  | BroadcastFailed [Int] [Int]
  deriving stock (Eq, Show, Functor)

data EvalErrorF sv
  = Bug (BugF sv)
  | AssertionFailure SpanInFile Ass1TypeVal Ass1TypeVal
  | RefinementAssertionFailure SpanInFile (Ass0ValF sv)
  deriving stock (Eq, Show, Functor)

type Bug = BugF StaticVar

type EvalError = EvalErrorF StaticVar
