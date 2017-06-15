module Data.TraversableWithIndex 
  ( class TraversableWithIndex, itraverse,
  , itraverseDefault
  , ifor_
  , iscanl
  , imapAccumL
  , iscanr
  , mapAccumR
  ) where

import Prelude

import Data.FunctorWithIndex (class FunctorWithIndex, imap)
import Data.Maybe (Maybe)
import Data.Maybe.First (First)
import Data.Maybe.Last (Last)
import Data.Monoid.Additive (Additive)
import Data.Monoid.Conj (Conj)
import Data.Monoid.Disj (Disj)
import Data.Monoid.Dual (Dual)
import Data.Monoid.Multiplicative (Multiplicative)
import Data.Traversable (class Traversable, Accum, sequence, traverse)
import Data.FoldableWithIndex (class FoldableWithIndex)

-- | A Traversable with an additional index.
-- | **TODO**: Laws. Apart from the IndexedTraversal laws of the haskell
-- | package (i.e. essentially the Traversal laws), we probably also want
-- | - `imap f = unwrap <<< itraverse (\i -> Identity <<< f i)`
-- | - `ifoldMap f = getConst <<< itraverse (\i -> Const <<< f i)`,
-- | i.e. that `imap` and `ifoldMap` are compatible with `itraverse`, and
-- | - `itraverse (const f) = traverse f
-- | i.e. that the `Traversable` and `IndexedTraversable` instances are
-- | compatible.
-- | Does it then follow that also
-- | - `sequence <<< imap f = itraverse f`
-- | ?
-- |
-- | A default implementation using `imap` and `sequence` is provided by
-- | `itraverseDefault`.
class (FunctorWithIndex i t, FoldableWithIndex i t, Traversable t) <= TraversableWithIndex i t | t -> i where
  -- TODO: formatting
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

foreign import itraverseArrayImpl
  :: forall m a b
   . (m (a -> b) -> m a -> m b)
  -> ((a -> b) -> m a -> m b)
  -> (a -> m a)
  -> (Int -> a -> m b)
  -> Array a
  -> m (Array b)

instance traversableWithIndexArray :: TraversableWithIndex Int Array where
  itraverse = itraverseArrayImpl apply map pure

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

newtype StateL s a = StateL (s -> Accum s a)

stateL :: forall s a. StateL s a -> s -> Accum s a
stateL (StateL k) = k

instance functorStateL :: Functor (StateL s) where
  map f k = StateL \s -> case stateL k s of
    { accum: s1, value: a } -> { accum: s1, value: f a }

instance applyStateL :: Apply (StateL s) where
  apply f x = StateL \s -> case stateL f s of
    { accum: s1, value: f' } -> case stateL x s1 of
      { accum: s2, value: x' } -> { accum: s2, value: f' x' }

instance applicativeStateL :: Applicative (StateL s) where
  pure a = StateL \s -> { accum: s, value: a }

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

newtype StateR s a = StateR (s -> Accum s a)

stateR :: forall s a. StateR s a -> s -> Accum s a
stateR (StateR k) = k

instance functorStateR :: Functor (StateR s) where
  map f k = StateR \s -> case stateR k s of
    { accum: s1, value: a } -> { accum: s1, value: f a }

instance applyStateR :: Apply (StateR s) where
  apply f x = StateR \s -> case stateR x s of
    { accum: s1, value: x' } -> case stateR f s1 of
      { accum: s2, value: f' } -> { accum: s2, value: f' x' }

instance applicativeStateR :: Applicative (StateR s) where
  pure a = StateR \s -> { accum: s, value: a }

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
  (mapAccumR (\i b a -> let b' = f i a b in { accum: b', value: b' }) b0 xs).value

-- | Fold a data structure from the right with access to the indices, keeping
-- | all intermediate results instead of only the final result.
-- |
-- | Unlike `iscanr`, `imapAccumR` allows the type of accumulator to differ
-- | from the element type of the final data structure.
mapAccumR
  :: forall i a b s f
   . TraversableWithIndex i f
  => (i -> s -> a -> Accum s b)
  -> s
  -> f a
  -> Accum s (f b)
mapAccumR f s0 xs = stateR (itraverse (\i a -> StateR \s -> f i s a) xs) s0
