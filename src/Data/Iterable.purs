module Data.Iterable where

import Prelude
import Control.Monad.Eff (Eff, foreachE)
import Data.Foldable (traverse_)
import Data.Traversable (traverse)
import Unsafe.Coerce (unsafeCoerce)

import Data.Maybe (Maybe)
import Data.Maybe.First (First)
import Data.Maybe.Last (Last)
import Data.Monoid.Additive (Additive)
import Data.Monoid.Conj (Conj)
import Data.Monoid.Disj (Disj)
import Data.Monoid.Dual (Dual)
import Data.Monoid.Multiplicative (Multiplicative)

-- | `Iterable` represents data structures which can be _traversed_,
-- | within the Eff monad.
-- |
-- | - `forEach` runs an effect for every element in a data structure,
-- |   and accumulates the results, in the manner of `traverse`.
-- | - `forEach_` runs an effect for every element in a data structure,
-- |   without result communication, in the manner of `traverse_`.
-- |
-- | While `forEach` and `forEach_` should be functionally equivalent
-- | to `traverse` and `traverse_`, respectively. The `Iterable` typeclass
-- | is provided such that optimized versions of these functions may
-- | be written for the Eff monad. For an example, see `iterableArray`,
-- | below.

class Iterable i where
  forEach :: forall eff a b. (a -> Eff eff b) -> i a -> Eff eff (i b)
  forEach_ :: forall eff a b. (a -> Eff eff b) -> i a -> Eff eff Unit

instance iterableArray :: Iterable Array where
  forEach_ f a = foreachE a $ unsafeCoerce f
  forEach = traverse

instance iterableMaybe :: Iterable Maybe where
  forEach_ = traverse_
  forEach = traverse

instance iterableFirst :: Iterable First where
  forEach_ = traverse_
  forEach = traverse

instance iterableLast :: Iterable Last where
  forEach_ = traverse_
  forEach = traverse

instance iterableAdditive :: Iterable Additive where
  forEach_ = traverse_
  forEach = traverse

instance iterableDual :: Iterable Dual where
  forEach_ = traverse_
  forEach = traverse

instance iterableConj :: Iterable Conj where
  forEach_ = traverse_
  forEach = traverse

instance iterableDisj :: Iterable Disj where
  forEach_ = traverse_
  forEach = traverse

instance iterableMultiplicative :: Iterable Multiplicative where
  forEach_ = traverse_
  forEach = traverse
