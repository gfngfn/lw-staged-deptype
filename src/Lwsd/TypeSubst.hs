module Lwsd.TypeSubst
  ( HasTypeVar (..),
  )
where

import Lwsd.Syntax
import Util.Maybe1
import Prelude

class HasTypeVar af where
  tySubst :: forall sv. Ass0TypeExprF sv -> AssTypeVar -> af sv -> af sv

instance (HasTypeVar af) => HasTypeVar (Maybe1 af) where
  tySubst a0tye' atyvar' = Maybe1 . fmap (tySubst a0tye' atyvar') . unMaybe1

instance HasTypeVar Ass0TypeExprF where
  tySubst :: forall sv. Ass0TypeExprF sv -> AssTypeVar -> Ass0TypeExprF sv -> Ass0TypeExprF sv
  tySubst a0tye' atyvar' = \case
    A0TyPrim a0tyPrim maybePred ->
      A0TyPrim a0tyPrim ((unMaybe1 . go . Maybe1) maybePred)
    A0TyVar atyvar ->
      if atyvar == atyvar' then a0tye' else A0TyVar atyvar
    A0TyList a0tye1 maybePred ->
      A0TyList (go a0tye1) ((unMaybe1 . go . Maybe1) maybePred)
    A0TyProduct a0tye1 a0tye2 ->
      A0TyProduct (go a0tye1) (go a0tye2)
    A0TyArrow (svOpt, a0tye1) a0tye2 ->
      A0TyArrow (svOpt, go a0tye1) (go a0tye2)
    A0TyOptArrow (ax, a0tye1) a0tye2 ->
      A0TyOptArrow (ax, go a0tye1) (go a0tye2)
    A0TyCode a1tye1 ->
      A0TyCode (go a1tye1)
    A0TyImplicitForAll atyvar a0tye1 ->
      A0TyImplicitForAll atyvar (if atyvar == atyvar' then a0tye1 else go a0tye1)
    where
      go :: forall af. (HasTypeVar af) => af sv -> af sv
      go = tySubst a0tye' atyvar'

instance HasTypeVar Ass1TypeExprF where
  tySubst :: forall sv. Ass0TypeExprF sv -> AssTypeVar -> Ass1TypeExprF sv -> Ass1TypeExprF sv
  tySubst a0tye' atyvar' = \case
    A1TyPrim a1tyPrim -> A1TyPrim a1tyPrim
    A1TyList a1tye1 -> A1TyList (go a1tye1)
    A1TyProduct a1tye1 a1tye2 -> A1TyProduct (go a1tye1) (go a1tye2)
    A1TyArrow a1tye1 a1tye2 -> A1TyArrow (go a1tye1) (go a1tye2)
    where
      go :: forall af. (HasTypeVar af) => af sv -> af sv
      go = tySubst a0tye' atyvar'

instance HasTypeVar Ass0ExprF where
  tySubst = error "TODO: HasTypeVar Ass0ExprF"
