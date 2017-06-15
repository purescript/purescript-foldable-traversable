module Data.FunctorWithIndex
  ( class FunctorWithIndex, imap
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Maybe.First (First)
import Data.Maybe.Last (Last)
import Data.Monoid.Additive (Additive)
import Data.Monoid.Conj (Conj)
import Data.Monoid.Disj (Disj)
import Data.Monoid.Dual (Dual)
import Data.Monoid.Multiplicative (Multiplicative)


-- | A `Functor` with an additional index.
-- | Instances must satisfy a modified form of the `Functor` laws: 
-- | 
-- | - Identity: `imap (\_ a -> a) = id`
-- | - Composition: `imap f . imap g = imap (\i -> f i <<< g i)`
-- |
-- | TODO: Don't we also want
-- | - `imap (const f) = map f`
-- | though?
-- |
-- | TODO: `imap` collides with `Invariant`s `imap`.
class Functor f <= FunctorWithIndex i f | f -> i where
  imap :: forall a b. (i -> a -> b) -> f a -> f b

foreign import imapArray :: forall i a b. (i -> a -> b) -> Array a -> Array b

instance functorWithIndexArray :: FunctorWithIndex Int Array where
  imap = imapArray

instance functorWithIndexMaybe :: FunctorWithIndex Unit Maybe where
  imap f = map $ f unit

instance functorWithIndexFirst :: FunctorWithIndex Unit First where
  imap f = map $ f unit

instance functorWithIndexLast :: FunctorWithIndex Unit Last where
  imap f = map $ f unit

instance functorWithIndexAdditive :: FunctorWithIndex Unit Additive where
  imap f = map $ f unit

instance functorWithIndexDual :: FunctorWithIndex Unit Dual where
  imap f = map $ f unit

instance functorWithIndexConj :: FunctorWithIndex Unit Conj where
  imap f = map $ f unit

instance functorWithIndexDisj :: FunctorWithIndex Unit Disj where
  imap f = map $ f unit

instance functorWithIndexMultiplicative :: FunctorWithIndex Unit Multiplicative where
  imap f = map $ f unit
