module Data.Foldable
  ( Foldable, foldr, foldl, foldMap
  , fold
  , traverse_
  , for_
  , sequence_
  , mconcat
  , intercalate
  , and
  , or
  , any
  , all
  , sum
  , product
  , elem
  , notElem
  , find
  ) where

import Prelude

import Control.Apply ((*>))
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..), runFirst)
import Data.Maybe.Last (Last(..))
import Data.Monoid (Monoid, mempty)
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Dual (Dual(..))
import Data.Monoid.Disj (Disj(..), runDisj)
import Data.Monoid.Conj (Conj(..), runConj)
import Data.Monoid.Multiplicative (Multiplicative(..))

-- | `Foldable` represents data structures which can be _folded_.
-- |
-- | - `foldr` folds a structure from the right
-- | - `foldl` folds a structure from the left
-- | - `foldMap` folds a structure by accumulating values in a `Monoid`
class Foldable f where
  foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldMap :: forall a m. (Monoid m) => (a -> m) -> f a -> m

instance foldableArray :: Foldable Array where
  foldr = foldrArray
  foldl = foldlArray
  foldMap f xs = foldr (\x acc -> f x <> acc) mempty xs

foreign import foldrArray :: forall a b. (a -> b -> b) -> b -> Array a -> b
foreign import foldlArray :: forall a b. (b -> a -> b) -> b -> Array a -> b

instance foldableMaybe :: Foldable Maybe where
  foldr _ z Nothing  = z
  foldr f z (Just x) = x `f` z
  foldl _ z Nothing  = z
  foldl f z (Just x) = z `f` x
  foldMap f Nothing  = mempty
  foldMap f (Just x) = f x

instance foldableFirst :: Foldable First where
  foldr f z (First x) = foldr f z x
  foldl f z (First x) = foldl f z x
  foldMap f (First x) = foldMap f x

instance foldableLast :: Foldable Last where
  foldr f z (Last x) = foldr f z x
  foldl f z (Last x) = foldl f z x
  foldMap f (Last x) = foldMap f x

instance foldableAdditive :: Foldable Additive where
  foldr f z (Additive x) = x `f` z
  foldl f z (Additive x) = z `f` x
  foldMap f (Additive x) = f x

instance foldableDual :: Foldable Dual where
  foldr f z (Dual x) = x `f` z
  foldl f z (Dual x) = z `f` x
  foldMap f (Dual x) = f x

instance foldableDisj :: Foldable Disj where
  foldr f z (Disj x) = f x z
  foldl f z (Disj x) = f z x
  foldMap f (Disj x) = f x

instance foldableConj :: Foldable Conj where
  foldr f z (Conj x) = f x z
  foldl f z (Conj x) = f z x
  foldMap f (Conj x) = f x

instance foldableMultiplicative :: Foldable Multiplicative where
  foldr f z (Multiplicative x) = x `f` z
  foldl f z (Multiplicative x) = z `f` x
  foldMap f (Multiplicative x) = f x

-- | Fold a data structure, accumulating values in some `Monoid`.
fold :: forall f m. (Foldable f, Monoid m) => f m -> m
fold = foldMap id

-- | Traverse a data structure, performing some effects encoded by an
-- | `Applicative` functor at each value, ignoring the final result.
-- |
-- | For example:
-- |
-- | ```purescript
-- | traverse_ print [1, 2, 3]
-- | ```
traverse_ :: forall a b f m. (Applicative m, Foldable f) => (a -> m b) -> f a -> m Unit
traverse_ f = foldr ((*>) <<< f) (pure unit)

-- | A version of `traverse_` with its arguments flipped.
-- |
-- | This can be useful when running an action written using do notation
-- | for every element in a data structure:
-- |
-- | For example:
-- |
-- | ```purescript
-- | for_ [1, 2, 3] \n -> do
-- |   print n
-- |   trace "squared is"
-- |   print (n * n)
-- | ```
for_ :: forall a b f m. (Applicative m, Foldable f) => f a -> (a -> m b) -> m Unit
for_ = flip traverse_

-- | Perform all of the effects in some data structure in the order
-- | given by the `Foldable` instance, ignoring the final result.
-- |
-- | For example:
-- |
-- | ```purescript
-- | sequence_ [ trace "Hello, ", trace " world!" ]
-- | ```
sequence_ :: forall a f m. (Applicative m, Foldable f) => f (m a) -> m Unit
sequence_ = traverse_ id

-- | Fold a data structure, accumulating values in some `Monoid`.
mconcat :: forall f m. (Foldable f, Monoid m) => f m -> m
mconcat = foldl (<>) mempty

-- | Fold a data structure, accumulating values in some `Monoid`,
-- | combining adjacent elements using the specified separator.
intercalate :: forall f m. (Foldable f, Monoid m) => m -> f m -> m
intercalate sep xs = (foldl go { init: true, acc: mempty } xs).acc
  where
  go { init = true } x = { init: false, acc: x }
  go { acc = acc }   x = { init: false, acc: acc <> sep <> x }

-- | Test whether all `Boolean` values in a data structure are `true`.
and :: forall a f. (Foldable f, BooleanAlgebra a) => f a -> a
and = all id

-- | Test whether any `Boolean` value in a data structure is `true`.
or :: forall a f. (Foldable f, BooleanAlgebra a) => f a -> a
or = any id

-- | Test whether a predicate holds for any element in a data structure.
any :: forall a b f. (Foldable f, BooleanAlgebra b) => (a -> b) -> f a -> b
any p = runDisj <<< foldMap (Disj <<< p)

-- | Test whether a predicate holds for all elements in a data structure.
all :: forall a b f. (Foldable f, BooleanAlgebra b) => (a -> b) -> f a -> b
all p = runConj <<< foldMap (Conj <<< p)

-- | Find the sum of the numeric values in a data structure.
sum :: forall a f. (Foldable f, Semiring a) => f a -> a
sum = foldl (+) zero

-- | Find the product of the numeric values in a data structure.
product :: forall a f. (Foldable f, Semiring a) => f a -> a
product = foldl (*) one

-- | Test whether a value is an element of a data structure.
elem :: forall a f. (Foldable f, Eq a) => a -> f a -> Boolean
elem = any <<< (==)

-- | Test whether a value is not an element of a data structure.
notElem :: forall a f. (Foldable f, Eq a) => a -> f a -> Boolean
notElem x = not <<< elem x

-- | Try to find an element in a data structure which satisfies a predicate.
find :: forall a f. (Foldable f) => (a -> Boolean) -> f a -> Maybe a
find p = foldl (\r x -> if p x then Just x else r) Nothing
