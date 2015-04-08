module Data.Foldable1 where

import Control.Apply
import Control.Functor
import Data.Foldable
import Data.Monoid.Additive
import Data.Monoid.Dual
import Data.Monoid.First
import Data.Monoid.Last
import Data.Monoid.Multiplicative
import Data.Tuple

class (Foldable t) <= Foldable1 t where
  foldMap1 :: forall a m. (Semigroup m) => (a -> m) -> t a -> m
  fold1 :: forall m. (Semigroup m) => t m -> m

derivedFold1 :: forall t m. (Foldable1 t, Semigroup m) => t m -> m
derivedFold1 = foldMap1 id

instance foldable1Tuple :: Foldable1 (Tuple a) where
  foldMap1 f (Tuple _ a) = f a
  fold1 = derivedFold1

instance foldableDual :: Foldable1 Dual where
  foldMap1 f (Dual x) = f x
  fold1 = derivedFold1

instance foldableMultiplicative :: Foldable1 Multiplicative where
  foldMap1 f (Multiplicative x) = f x
  fold1 = derivedFold1

newtype Act f a = Act (f a)

getAct :: forall f a. Act f a -> f a
getAct (Act f) = f

instance semigroupAct :: (Apply f) => Semigroup (Act f a) where
  (<>) (Act a) (Act b) = Act (a *> b)

traverse1_ :: forall t f a b. (Foldable1 t, Apply f) => (a -> f b) -> t a -> f Unit
traverse1_ f t = unit <$ getAct (foldMap1 (Act <<< f) t)

for1_ :: forall t f a b. (Foldable1 t, Apply f) => t a -> (a -> f b) -> f Unit
for1_ = flip traverse1_

sequence1_ :: forall t f a. (Foldable1 t, Apply f) => t (f a) -> f Unit
sequence1_ = traverse1_ id
