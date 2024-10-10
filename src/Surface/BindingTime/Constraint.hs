module Surface.BindingTime.Constraint
  ( Constraint (..),
    BindingTimeSubst,
    solveConstraints,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Surface.BindingTime.Core
import Prelude

data Constraint ann
  = CLeq ann BindingTime BindingTime
  | CEqual ann BindingTime BindingTime
  deriving stock (Show)

type BindingTimeSubst = Map BindingTimeVar BindingTime

data SolvingStepResult ann
  = NotFound
  | Subst BindingTimeVar BindingTime [Constraint ann]
  | TrivialEliminated [Constraint ann]
  | ContradictionDetected ann

type M ann a = Either ann a

err :: ann -> M ann a
err = Left

solveConstraints :: forall ann. [Constraint ann] -> M ann (BindingTimeSubst, [Constraint ann])
solveConstraints = go Map.empty
  where
    go :: BindingTimeSubst -> [Constraint ann] -> M ann (BindingTimeSubst, [Constraint ann])
    go accMap constraints =
      case step [] constraints of
        NotFound ->
          pure (accMap, constraints)
        Subst btvFrom btTo remainingConstraints -> do
          let accMapNew = Map.insert btvFrom btTo (substSubst btvFrom btTo accMap)
          let constraintsNew = map (substConstraint btvFrom btTo) remainingConstraints
          go accMapNew constraintsNew
        TrivialEliminated constraintsNew ->
          go accMap constraintsNew
        ContradictionDetected ann -> do
          err ann

    step :: [Constraint ann] -> [Constraint ann] -> SolvingStepResult ann
    step constraintAcc = \case
      [] -> NotFound
      CLeq _ann (BTVar btv1) (BTVar btv2) : rest | btv1 == btv2 -> TrivialEliminated (reverse constraintAcc ++ rest)
      CLeq _ann (BTVar btv1) (BTConst BT0) : rest -> Subst btv1 (BTConst BT0) (reverse constraintAcc ++ rest)
      CLeq _ann (BTConst BT1) (BTVar btv2) : rest -> Subst btv2 (BTConst BT1) (reverse constraintAcc ++ rest)
      CLeq _ann (BTConst BT0) _ : rest -> TrivialEliminated (reverse constraintAcc ++ rest)
      CLeq _ann _ (BTConst BT1) : rest -> TrivialEliminated (reverse constraintAcc ++ rest)
      CLeq ann (BTConst BT1) (BTConst BT0) : _ -> ContradictionDetected ann
      constraint@(CLeq _ann (BTVar _) (BTVar _)) : rest -> step (constraint : constraintAcc) rest
      CEqual _ann (BTVar btv1) (BTVar btv2) : rest | btv1 == btv2 -> TrivialEliminated (reverse constraintAcc ++ rest)
      CEqual _ann (BTVar btv1) bt2 : rest -> Subst btv1 bt2 (reverse constraintAcc ++ rest)
      CEqual _ann bt1 (BTVar btv2) : rest -> Subst btv2 bt1 (reverse constraintAcc ++ rest)
      CEqual ann (BTConst btc1) (BTConst btc2) : rest ->
        if btc1 == btc2
          then TrivialEliminated (reverse constraintAcc ++ rest)
          else ContradictionDetected ann

    substSubst :: BindingTimeVar -> BindingTime -> BindingTimeSubst -> BindingTimeSubst
    substSubst btvFrom btTo =
      Map.map (substBindingTime btvFrom btTo)

    substConstraint :: BindingTimeVar -> BindingTime -> Constraint ann -> Constraint ann
    substConstraint btvFrom btTo = \case
      CLeq ann bt1 bt2 -> CLeq ann (f bt1) (f bt2)
      CEqual ann bt1 bt2 -> CEqual ann (f bt1) (f bt2)
      where
        f = substBindingTime btvFrom btTo

    substBindingTime :: BindingTimeVar -> BindingTime -> BindingTime -> BindingTime
    substBindingTime btvFrom btTo bt =
      case bt of
        BTVar btv -> if btv == btvFrom then btTo else bt
        BTConst _ -> bt
