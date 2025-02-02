module Lwsd.BuiltIn
  ( tyNat,
  )
where

import Lwsd.BuiltIn.Core
import Lwsd.Syntax
import Prelude

ass0exprIsNonnegative :: Ass0Expr
ass0exprIsNonnegative =
  A0App (A0BuiltInName (BuiltInArity2 BILeq)) (A0Literal (ALitInt 0))

tyNat :: Ass0TypeExpr
tyNat = A0TyPrim A0TyInt (Just ass0exprIsNonnegative)
