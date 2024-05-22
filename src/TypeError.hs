module TypeError
  ( TypeError (..),
  )
where

import Syntax

data TypeError
  = UnboundVar Var
  | UnknownTypeOrInvalidArity TypeName Int
  | TypeContradiction TypeExpr TypeExpr
  | NotAFunctionType TypeExpr
  deriving stock (Show)
