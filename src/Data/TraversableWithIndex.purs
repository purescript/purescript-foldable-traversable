module Data.TraversableWithIndex 
  ( class TraversableWithIndex, itraverse
  , itraverseDefault
  , ifor
  , iscanl
  , imapAccumL
  , iscanr
  , imapAccumR
  , module Data.Traversable.Accum
  ) where

import Prelude

import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, imap)
import Data.Maybe (Maybe)
import Data.Maybe.First (First)
import Data.Maybe.Last (Last)
import Data.Monoid.Additive (Additive)
import Data.Monoid.Conj (Conj)
import Data.Monoid.Disj (Disj)
import Data.Monoid.Dual (Dual)
import Data.Monoid.Multiplicative (Multiplicative)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Traversable.Accum (Accum)
import Data.Traversable.Accum.Internal (StateL(..), StateR(..), stateL, stateR)


-- | A `Traversable` with an additional index.  
-- | A `TraversableWithIndex` instance must be compatible with its
-- | `Traversable` instance
-- | ```purescript
-- | traverse f = itraverse (const f)
-- | ```
-- | with its `FoldableWithIndex` instance
-- | ```
-- | ifoldMap f = unwrap <<< itraverse (\i -> Const <<< f i)
-- | ```
-- | and with its `FunctorWithIndex` instance
-- | ```
-- | imap f = unwrap <<< itraverse (\i -> Identity <<< f i)
-- | ```
-- |
-- | A default implementation is provided by `itraverseDefault`.
class (FunctorWithIndex i t, FoldableWithIndex i t, Traversable t) <= TraversableWithIndex i t | t -> i where
  itraverse :: forall a b m. Applicative m => (i -> a -> m b) -> t a -> m (t b)

-- | A default implementation of `itraverse` using `sequence` and `imap`.
itraverseDefault
  :: forall i t a b m
   . TraversableWithIndex i t
  => Applicative m
  => (i -> a -> m b)
  -> t a
  -> m (t b)
itraverseDefault f = sequence <<< imap f

instance traversableWithIndexArray :: TraversableWithIndex Int Array where
  itraverse = itraverseDefault

instance traversableWithIndexMaybe :: TraversableWithIndex Unit Maybe where
  itraverse f = traverse $ f unit

instance traversableWithIndexFirst :: TraversableWithIndex Unit First where
  itraverse f = traverse $ f unit

instance traversableWithIndexLast :: TraversableWithIndex Unit Last where
  itraverse f = traverse $ f unit

instance traversableWithIndexAdditive :: TraversableWithIndex Unit Additive where
  itraverse f = traverse $ f unit

instance traversableWithIndexDual :: TraversableWithIndex Unit Dual where
  itraverse f = traverse $ f unit

instance traversableWithIndexConj :: TraversableWithIndex Unit Conj where
  itraverse f = traverse $ f unit

instance traversableWithIndexDisj :: TraversableWithIndex Unit Disj where
  itraverse f = traverse $ f unit

instance traversableWithIndexMultiplicative :: TraversableWithIndex Unit Multiplicative where
  itraverse f = traverse $ f unit

-- | A version of `itraverse` with its arguments flipped.
-- |
-- |
-- | This can be useful when running an action written using do notation
-- | for every element in a data structure:
-- |
-- | For example:
-- |
-- | ```purescript
-- | for [1, 2, 3] \i x -> do
-- |   logShow i
-- |   pure (x * x)
-- | ```
ifor
  :: forall i a b m t
   . Applicative m
  => TraversableWithIndex i t
  => t a
  -> (i -> a -> m b)
  -> m (t b)
ifor = flip itraverse

-- | Fold a data structure from the left with access to the indices, keeping
-- | all intermediate results instead of only the final result. Note that the
-- | initial value does not appear in the result (unlike Haskell's
-- | `Prelude.scanl`).
-- |
-- | ```purescript
-- | iscanl (\i y x -> i + y + x) 0 [1, 2, 3] = [1, 4, 9]
-- | ```
iscanl
  :: forall i a b f
   . TraversableWithIndex i f
  => (i -> b -> a -> b)
  -> b
  -> f a
  -> f b
iscanl f b0 xs =
  (imapAccumL (\i b a -> let b' = f i b a in { accum: b', value: b' }) b0 xs).value

-- | Fold a data structure from the left with access to the indices, keeping
-- | all intermediate results instead of only the final result.
-- |
-- | Unlike `iscanl`, `imapAccumL` allows the type of accumulator to differ
-- | from the element type of the final data structure.
imapAccumL
  :: forall i a b s f
   . TraversableWithIndex i f
  => (i -> s -> a -> Accum s b)
  -> s
  -> f a
  -> Accum s (f b)
imapAccumL f s0 xs = stateL (itraverse (\i a -> StateL \s -> f i s a) xs) s0

-- | Fold a data structure from the right with access to the indices, keeping
-- | all intermediate results instead of only the final result. Note that the
-- | initial value does not appear in the result (unlike Haskell's `Prelude.scanr`).
-- |
-- | ```purescript
-- | iscanr (\i x y -> i + x + y) 0 [1, 2, 3] = [9, 8, 5]
-- | ```
iscanr
  :: forall i a b f
   . TraversableWithIndex i f
  => (i -> a -> b -> b)
  -> b
  -> f a
  -> f b
iscanr f b0 xs =
  (imapAccumR (\i b a -> let b' = f i a b in { accum: b', value: b' }) b0 xs).value

-- | Fold a data structure from the right with access to the indices, keeping
-- | all intermediate results instead of only the final result.
-- |
-- | Unlike `iscanr`, `iimapAccumR` allows the type of accumulator to differ
-- | from the element type of the final data structure.
imapAccumR
  :: forall i a b s f
   . TraversableWithIndex i f
  => (i -> s -> a -> Accum s b)
  -> s
  -> f a
  -> Accum s (f b)
imapAccumR f s0 xs = stateR (itraverse (\i a -> StateR \s -> f i s a) xs) s0
