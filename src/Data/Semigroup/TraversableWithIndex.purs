module Data.Semigroup.TraversableWithIndex where

import Prelude

import Data.FunctorWithIndex (mapWithIndex)
import Data.Identity (Identity(..))
import Data.Monoid.Dual (Dual(..))
import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Semigroup.FoldableWithIndex (class FoldableWithIndex1)
import Data.Semigroup.Traversable (class Traversable1)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Tuple (Tuple(..))

-- | `TraversableWithIndex1` represents non-empty data structures with indices that can be _traversed_,
-- | accumulating results and effects in some `Apply` functor.
-- |
-- | - `traverseWithIndex1` runs an action for every element in a data structure
-- |   with access to indices, and accumulates the results.
-- | - `sequenceWithIndex1` runs the actions _contained_ in a data structure,
-- |   and accumulates the results.
-- |
-- | This is the indexed version of `Traversable1`, for structures with at least one element.
-- |
-- | The `traverseWithIndex1` and `sequenceWithIndex1` functions should be compatible in the
-- | following sense:
-- |
-- | - `traverseWithIndex1 f xs = sequenceWithIndex1 (mapWithIndex f xs)`
-- | - `sequenceWithIndex1 = traverseWithIndex1 (const identity)`
-- |
-- | `TraversableWithIndex1` instances should also be compatible with the corresponding
-- | `FoldableWithIndex1` instances, in the following sense:
-- |
-- | - `foldMapWithIndex1 f = runConst <<< traverseWithIndex1 (\i -> Const <<< f i)`
-- |
-- | And with the corresponding `Traversable1` instances:
-- |
-- | - `traverse1 f = traverseWithIndex1 (const f)`
-- |
-- | Default implementations are provided by the following functions:
-- |
-- | - `traverseWithIndex1Default`
-- | - `sequenceWithIndex1Default`
class (FoldableWithIndex1 i t, TraversableWithIndex i t, Traversable1 t) <= TraversableWithIndex1 i t | t -> i where
  traverseWithIndex1 :: forall a b f. Apply f => (i -> a -> f b) -> t a -> f (t b)
  sequenceWithIndex1 :: forall b f. Apply f => t (f b) -> f (t b)

instance traversableWithIndex1Dual :: TraversableWithIndex1 Unit Dual where
  traverseWithIndex1 f (Dual x) = Dual <$> f unit x
  sequenceWithIndex1 = sequenceWithIndex1Default

instance traversableWithIndex1Multiplicative :: TraversableWithIndex1 Unit Multiplicative where
  traverseWithIndex1 f (Multiplicative x) = Multiplicative <$> f unit x
  sequenceWithIndex1 = sequenceWithIndex1Default

instance traversableWithIndex1Tuple :: TraversableWithIndex1 Unit (Tuple a) where
  traverseWithIndex1 f (Tuple x y) = Tuple x <$> f unit y
  sequenceWithIndex1 (Tuple x y) = Tuple x <$> y

instance traversableWithIndex1Identity :: TraversableWithIndex1 Unit Identity where
  traverseWithIndex1 f (Identity x) = Identity <$> f unit x
  sequenceWithIndex1 (Identity x) = Identity <$> x

-- | A default implementation of `traverseWithIndex1` using `sequenceWithIndex1` and `mapWithIndex`.
traverseWithIndex1Default
  :: forall t i a b m
   . TraversableWithIndex1 i t
  => Apply m
  => (i -> a -> m b)
  -> t a
  -> m (t b)
traverseWithIndex1Default f ta = sequenceWithIndex1 (mapWithIndex f ta)

-- | A default implementation of `sequenceWithIndex1` using `traverseWithIndex1`.
sequenceWithIndex1Default
  :: forall t i a m
   . TraversableWithIndex1 i t
  => Apply m
  => t (m a)
  -> m (t a)
sequenceWithIndex1Default = traverseWithIndex1 (const identity)

-- | A version of `traverseWithIndex1` with its arguments flipped.
-- |
-- | This can be useful when running an action written using do notation
-- | for every element in a data structure with access to indices:
forWithIndex1
  :: forall i a b m t
   . Apply m
  => TraversableWithIndex1 i t
  => t a
  -> (i -> a -> m b)
  -> m (t b)
forWithIndex1 = flip traverseWithIndex1

-- | A default implementation of `traverse1` in terms of `traverseWithIndex1`
traverse1Default
  :: forall i t a b m
   . TraversableWithIndex1 i t
  => Apply m
  => (a -> m b) -> t a -> m (t b)
traverse1Default f = traverseWithIndex1 (const f)

-- | A default implementation of `traverseWithIndex` in terms of `traverseWithIndex1`
-- | This works because `TraversableWithIndex1` extends `TraversableWithIndex`
traverseWithIndexDefault
  :: forall i t a b m
   . TraversableWithIndex1 i t
  => Applicative m
  => (i -> a -> m b) -> t a -> m (t b)
traverseWithIndexDefault f = traverseWithIndex1 f
