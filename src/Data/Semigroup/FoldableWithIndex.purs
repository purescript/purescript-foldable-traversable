module Data.Semigroup.FoldableWithIndex
( class FoldableWithIndex1
, foldMapWithIndex1
, foldWithIndex1
, foldrWithIndex1
, foldlWithIndex1
, traverseWithIndex1_
, forWithIndex1_
, sequenceWithIndex1_
-- , foldrWithIndex1Default
-- , foldlWithIndex1Default
, foldMapWithIndex1DefaultR
, foldMapWithIndex1DefaultL
, intercalateWithIndex
, intercalateMapWithIndex
, maximumByWithIndex
, minimumByWithIndex
) where

import Prelude

import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Identity (Identity(..))
import Data.Monoid.Dual (Dual(..))
import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Semigroup.Foldable (class Foldable1)
import Data.Semigroup.Foldable.Internal (Act(..), getAct)
import Data.Tuple (Tuple(..))

-- | `FoldableWithIndex1` represents non-empty data structures with indices that can be _folded_.
-- |
-- | - `foldrWithIndex1` folds a structure from the right with access to indices
-- | - `foldlWithIndex1` folds a structure from the left with access to indices
-- | - `foldMapWithIndex1` folds a structure by accumulating values in a `Semigroup` with access to indices
-- |
-- | This is the indexed version of `Foldable1`, for structures with at least one element.
-- |
-- | Default implementations are provided by the following functions:
-- |
-- | - `foldrWithIndex1Default`
-- | - `foldlWithIndex1Default`
-- | - `foldMapWithIndex1DefaultR`
-- | - `foldMapWithIndex1DefaultL`
-- |
-- | Note: some combinations of the default implementations are unsafe to
-- | use together - causing a non-terminating mutually recursive cycle.
-- | These combinations are documented per function.
class (FoldableWithIndex i t, Foldable1 t) <= FoldableWithIndex1 i t | t -> i where
  foldrWithIndex1 :: forall a. (i -> a -> a -> a) -> t a -> a
  foldlWithIndex1 :: forall a. (i -> a -> a -> a) -> t a -> a
  foldMapWithIndex1 :: forall a m. Semigroup m => (i -> a -> m) -> t a -> m

-- | A default implementation of `foldrWithIndex1` using `foldMapWithIndex1`.
-- |
-- | Note: when defining a `FoldableWithIndex1` instance, this function is unsafe to use
-- | in combination with `foldMapWithIndex1DefaultR`.
-- foldrWithIndex1Default :: forall t i a. FoldableWithIndex1 i t => (i -> a -> a -> a) -> t a -> a
-- foldrWithIndex1Default f = flip runFoldRightWithIndex1 f <<< foldMapWithIndex1 (curry mkFoldRightWithIndex1)

-- | A default implementation of `foldlWithIndex1` using `foldMapWithIndex1`.
-- |
-- | Note: when defining a `FoldableWithIndex1` instance, this function is unsafe to use
-- | in combination with `foldMapWithIndex1DefaultL`.
-- foldlWithIndex1Default :: forall t i a. FoldableWithIndex1 i t => (i -> a -> a -> a) -> t a -> a
-- foldlWithIndex1Default f = runFoldRightWithIndex1 <<< alaF Dual foldMapWithIndex1 (curry mkFoldRightWithIndex1) <<< flip <<< f

-- | A default implementation of `foldMapWithIndex1` using `foldrWithIndex1`.
-- |
-- | Note: when defining a `FoldableWithIndex1` instance, this function is unsafe to use
-- | in combination with `foldrWithIndex1Default`.
foldMapWithIndex1DefaultR :: forall t i m a. FoldableWithIndex1 i t => FunctorWithIndex i t => Semigroup m => (i -> a -> m) -> t a -> m
foldMapWithIndex1DefaultR f t = foldrWithIndex1 (\_i m1 m2 -> m1 <> m2) (mapWithIndex f t :: t m)

-- | A default implementation of `foldMapWithIndex1` using `foldlWithIndex1`.
-- |
-- | Note: when defining a `FoldableWithIndex1` instance, this function is unsafe to use
-- | in combination with `foldlWithIndex1Default`.
foldMapWithIndex1DefaultL :: forall t i m a. FoldableWithIndex1 i t => FunctorWithIndex i t => Semigroup m => (i -> a -> m) -> t a -> m
foldMapWithIndex1DefaultL f t = foldlWithIndex1 (\_i m1 m2 -> m1 <> m2) (mapWithIndex f t)

instance foldableWithIndex1Dual :: FoldableWithIndex1 Unit Dual where
  foldrWithIndex1 _ (Dual x) = x
  foldlWithIndex1 _ (Dual x) = x
  foldMapWithIndex1 f (Dual x) = f unit x

instance foldableWithIndex1Multiplicative :: FoldableWithIndex1 Unit Multiplicative where
  foldrWithIndex1 _ (Multiplicative x) = x
  foldlWithIndex1 _ (Multiplicative x) = x
  foldMapWithIndex1 f (Multiplicative x) = f unit x

instance foldableWithIndex1Tuple :: FoldableWithIndex1 Unit (Tuple a) where
  foldMapWithIndex1 f (Tuple _ x) = f unit x
  foldrWithIndex1 _ (Tuple _ x) = x
  foldlWithIndex1 _ (Tuple _ x) = x

instance foldableWithIndex1Identity :: FoldableWithIndex1 Unit Identity where
  foldMapWithIndex1 f (Identity x) = f unit x
  foldlWithIndex1 _ (Identity x) = x
  foldrWithIndex1 _ (Identity x) = x

-- | Fold a data structure with access to indices, accumulating values in some `Semigroup`.
foldWithIndex1 :: forall t i m. FoldableWithIndex1 i t => Semigroup m => t m -> m
foldWithIndex1 = foldMapWithIndex1 (const identity)

-- | Traverse a data structure with access to indices, performing some effects encoded by an
-- | `Apply` instance at each value, ignoring the final result.
traverseWithIndex1_ :: forall t i f a b. FoldableWithIndex1 i t => Apply f => (i -> a -> f b) -> t a -> f Unit
traverseWithIndex1_ f t = unit <$ getAct (foldMapWithIndex1 (\i -> Act <<< f i) t)

-- | A version of `traverseWithIndex1_` with its arguments flipped.
-- |
-- | This can be useful when running an action written using do notation
-- | for every element in a data structure with access to indices:
forWithIndex1_ :: forall t i f a b. FoldableWithIndex1 i t => Apply f => t a -> (i -> a -> f b) -> f Unit
forWithIndex1_ = flip traverseWithIndex1_

-- | Perform all of the effects in some data structure in the order
-- | given by the `FoldableWithIndex1` instance, ignoring the final result.
sequenceWithIndex1_ :: forall t i f a. FoldableWithIndex1 i t => Apply f => t (f a) -> f Unit
sequenceWithIndex1_ = traverseWithIndex1_ (const identity)

-- | Find the maximum element of a structure with access to indices, according to a given comparison
-- | function. The comparison function receives the indices and elements.
maximumByWithIndex :: forall f i a. FoldableWithIndex1 i f => (i -> a -> a -> Ordering) -> f a -> a
maximumByWithIndex cmp = foldlWithIndex1 \i x y -> if cmp i x y == GT then x else y

-- | Find the minimum element of a structure with access to indices, according to a given comparison
-- | function. The comparison function receives the indices and elements.
minimumByWithIndex :: forall f i a. FoldableWithIndex1 i f => (i -> a -> a -> Ordering) -> f a -> a
minimumByWithIndex cmp = foldlWithIndex1 \i x y -> if cmp i x y == LT then x else y

-- | Internal. Used by intercalation functions with indices.
newtype JoinWithIndex i a = JoinWithIndex (i -> a -> a)

joineeWithIndex :: forall i a. JoinWithIndex i a -> i -> a -> a
joineeWithIndex (JoinWithIndex x) = x

instance semigroupJoinWithIndex :: Semigroup a => Semigroup (JoinWithIndex i a) where
  append (JoinWithIndex a) (JoinWithIndex b) = JoinWithIndex $ \i j -> a i j <> j <> b i j

-- | Fold a data structure using a `Semigroup` instance with access to indices,
-- | combining adjacent elements using the specified separator.
intercalateWithIndex :: forall f i m. FoldableWithIndex1 i f => Semigroup m => m -> f m -> m
intercalateWithIndex = flip intercalateMapWithIndex (const identity)

-- | Fold a data structure, accumulating values in some `Semigroup` with access to indices,
-- | combining adjacent elements using the specified separator.
intercalateMapWithIndex
  :: forall f i m a
  . FoldableWithIndex1 i f
  => Semigroup m
  => m -> (i -> a -> m) -> f a -> m
intercalateMapWithIndex j f foldable =
  joineeWithIndex (foldMapWithIndex1 (\i -> JoinWithIndex <<< const <<< const <<< f i) foldable) unit j

-- | Internal. Used by foldrWithIndex1Default and foldlWithIndex1Default.
data FoldRightWithIndex1 i a = FoldRightWithIndex1 (a -> (i -> a -> a -> a) -> a) a

-- instance foldRightWithIndex1Semigroup :: Semigroup (FoldRightWithIndex1 i a) where
--   append (FoldRightWithIndex1 lf lr) (FoldRightWithIndex1 rf rr) = FoldRightWithIndex1 (\a f -> lf (f lr (rf a f)) f) rr -- FIXME: An infinite type was inferred for an expression

mkFoldRightWithIndex1 :: forall i a. Tuple i a -> FoldRightWithIndex1 i a
mkFoldRightWithIndex1 (Tuple _ a) = FoldRightWithIndex1 const a

runFoldRightWithIndex1 :: forall i a. FoldRightWithIndex1 i a -> (i -> a -> a -> a) -> a
runFoldRightWithIndex1 (FoldRightWithIndex1 f a) = f a
