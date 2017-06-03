module Data.Semigroup.Foldable where

import Prelude
import Data.Foldable (class Foldable)
import Data.Monoid.Dual (Dual(..))
import Data.Monoid.Multiplicative (Multiplicative(..))

-- | `Foldable1` represents data structures with a minimum of one element that can be _folded_.
-- |
-- | - `fold1` folds a structure using a `Semigroup` instance
-- | - `foldMap1` folds a structure by accumulating values in a `Semigroup`
-- |
-- | Default implementations are provided by the following functions:
-- |
-- | - `foldrDefault`
-- | - `foldlDefault`
-- | - `foldMapDefaultR`
-- | - `foldMapDefaultL`
-- |
-- | Note: some combinations of the default implementations are unsafe to
-- | use together - causing a non-terminating mutually recursive cycle.
-- | These combinations are documented per function.
class (Foldable t) <= Foldable1 t where
  foldMap1 :: forall a m. (Semigroup m) => (a -> m) -> t a -> m
  fold1 :: forall m. (Semigroup m) => t m -> m

fold1Default :: forall t m. Foldable1 t => Semigroup m => t m -> m
fold1Default = foldMap1 id

foldMap1Default :: forall t m a. Foldable1 t => Functor t => Semigroup m => (a -> m) -> t a -> m
foldMap1Default f = (map f) >>> fold1

instance foldableDual :: Foldable1 Dual where
  foldMap1 f (Dual x) = f x
  fold1 = fold1Default

instance foldableMultiplicative :: Foldable1 Multiplicative where
  foldMap1 f (Multiplicative x) = f x
  fold1 = fold1Default

newtype Act f a = Act (f a)

getAct :: forall f a. Act f a -> f a
getAct (Act f) = f

instance semigroupAct :: (Apply f) => Semigroup (Act f a) where
  append (Act a) (Act b) = Act (a *> b)

traverse1_ :: forall t f a b. Foldable1 t => Apply f => (a -> f b) -> t a -> f Unit
traverse1_ f t = unit <$ getAct (foldMap1 (Act <<< f) t)

for1_ :: forall t f a b. Foldable1 t => Apply f => t a -> (a -> f b) -> f Unit
for1_ = flip traverse1_

sequence1_ :: forall t f a. Foldable1 t => Apply f => t (f a) -> f Unit
sequence1_ = traverse1_ id
