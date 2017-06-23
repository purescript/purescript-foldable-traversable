module Data.FoldableWithIndex
  ( class FoldableWithIndex, ifoldr, ifoldl, ifoldMap
  , ifoldrDefault
  , ifoldlDefault
  , ifoldMapDefaultR
  , ifoldMapDefaultL
  , ifoldM
  , itraverse_
  , ifor_
  , isurroundMap
  , iall
  , iany
  , ifind
  ) where

import Prelude

import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.FunctorWithIndex (imap)
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First)
import Data.Maybe.Last (Last)
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Additive (Additive)
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Monoid.Dual (Dual(..))
import Data.Monoid.Endo (Endo(..))
import Data.Monoid.Multiplicative (Multiplicative)
import Data.Newtype (unwrap)

-- | A `Foldable` with an additional index.  
-- | A `FoldableWithIndex` instance must be compatible with its `Foldable`
-- | instance
-- | ```purescript
-- | foldr f = ifoldr (const f)
-- | foldl f = ifoldl (const f)
-- | foldMap f = ifoldMap (const f)
-- | ```
-- | 
-- | Default implementations are provided by the following functions:
-- |
-- | - `ifoldrDefault`
-- | - `ifoldlDefault`
-- | - `ifoldMapDefaultR`
-- | - `ifoldMapDefaultL`
-- |
-- | Note: some combinations of the default implementations are unsafe to
-- | use together - causing a non-terminating mutually recursive cycle.
-- | These combinations are documented per function.
class Foldable f <= FoldableWithIndex i f | f -> i where
  ifoldr :: forall a b. (i -> a -> b -> b) -> b -> f a -> b
  ifoldl :: forall a b. (i -> b -> a -> b) -> b -> f a -> b
  ifoldMap :: forall a m. Monoid m => (i -> a -> m) -> f a -> m

-- | A default implementation of `ifoldr` using `ifoldMap`.
-- |
-- | Note: when defining a `FoldableWithIndex` instance, this function is
-- | unsafe to use in combination with `ifoldMapDefaultR`.
ifoldrDefault
  :: forall i f a b
   . FoldableWithIndex i f
  => (i -> a -> b -> b)
  -> b
  -> f a
  -> b
ifoldrDefault c u xs = unwrap (ifoldMap (\i -> Endo <<< c i) xs) u

-- | A default implementation of `ifoldl` using `ifoldMap`.
-- |
-- | Note: when defining a `FoldableWithIndex` instance, this function is
-- | unsafe to use in combination with `ifoldMapDefaultL`.
ifoldlDefault
  :: forall i f a b
   . FoldableWithIndex i f
  => (i -> b -> a -> b)
  -> b
  -> f a
  -> b
ifoldlDefault c u xs = unwrap (unwrap (ifoldMap (\i -> Dual <<< Endo <<< flip (c i)) xs)) u

-- | A default implementation of `ifoldMap` using `ifoldr`.
-- |
-- | Note: when defining a `FoldableWithIndex` instance, this function is
-- | unsafe to use in combination with `ifoldrDefault`.
ifoldMapDefaultR
  :: forall i f a m
   . FoldableWithIndex i f
  => Monoid m
  => (i -> a -> m)
  -> f a
  -> m
ifoldMapDefaultR f = ifoldr (\i x acc -> f i x <> acc) mempty

-- | A default implementation of `ifoldMap` using `ifoldl`.
-- |
-- | Note: when defining a `FoldableWithIndex` instance, this function is
-- | unsafe to use in combination with `ifoldlDefault`.
ifoldMapDefaultL
  :: forall i f a m
   . FoldableWithIndex i f
  => Monoid m
  => (i -> a -> m)
  -> f a
  -> m
ifoldMapDefaultL f = ifoldl (\i acc x -> acc <> f i x) mempty

data Tuple a b = Tuple a b

instance foldableWithIndexArray :: FoldableWithIndex Int Array where
  ifoldr f z = foldr (\(Tuple i x) y -> f i x y) z <<< imap Tuple
  ifoldl f z = foldl (\y (Tuple i x) -> f i y x) z <<< imap Tuple
  ifoldMap = ifoldMapDefaultR

instance foldableWithIndexMaybe :: FoldableWithIndex Unit Maybe where
  ifoldr f = foldr $ f unit
  ifoldl f = foldl $ f unit
  ifoldMap f = foldMap $ f unit

instance foldableWithIndexFirst :: FoldableWithIndex Unit First where
  ifoldr f = foldr $ f unit
  ifoldl f = foldl $ f unit
  ifoldMap f = foldMap $ f unit

instance foldableWithIndexLast :: FoldableWithIndex Unit Last where
  ifoldr f = foldr $ f unit
  ifoldl f = foldl $ f unit
  ifoldMap f = foldMap $ f unit

instance foldableWithIndexAdditive :: FoldableWithIndex Unit Additive where
  ifoldr f = foldr $ f unit
  ifoldl f = foldl $ f unit
  ifoldMap f = foldMap $ f unit

instance foldableWithIndexDual :: FoldableWithIndex Unit Dual where
  ifoldr f = foldr $ f unit
  ifoldl f = foldl $ f unit
  ifoldMap f = foldMap $ f unit

instance foldableWithIndexDisj :: FoldableWithIndex Unit Disj where
  ifoldr f = foldr $ f unit
  ifoldl f = foldl $ f unit
  ifoldMap f = foldMap $ f unit

instance foldableWithIndexConj :: FoldableWithIndex Unit Conj where
  ifoldr f = foldr $ f unit
  ifoldl f = foldl $ f unit
  ifoldMap f = foldMap $ f unit

instance foldableWithIndexMultiplicative :: FoldableWithIndex Unit Multiplicative where
  ifoldr f = foldr $ f unit
  ifoldl f = foldl $ f unit
  ifoldMap f = foldMap $ f unit


-- | Similar to 'ifoldl', but the result is encapsulated in a monad. 
-- |
-- | Note: this function is not generally stack-safe, e.g., for monads which 
-- | build up thunks a la `Eff`.
ifoldM
  :: forall i f m a b
   . FoldableWithIndex i f
  => Monad m
  => (i -> a -> b -> m a)
  -> a
  -> f b
  -> m a
ifoldM f a0 = ifoldl (\i ma b -> ma >>= flip (f i) b) (pure a0)

-- | Traverse a data structure with access to the index, performing some
-- | effects encoded by an `Applicative` functor at each value, ignoring the
-- | final result.
-- |
-- | For example:
-- |
-- | ```purescript
-- | > itraverse_ (curry logShow) ["a", "b", "c"]
-- | (Tuple 0 "a")
-- | (Tuple 1 "b")
-- | (Tuple 2 "c")
-- | ```
itraverse_
  :: forall i a b f m
   . Applicative m
  => FoldableWithIndex i f
  => (i -> a -> m b)
  -> f a
  -> m Unit
itraverse_ f = ifoldr (\i -> (*>) <<< f i) (pure unit)

-- | A version of `itraverse_` with its arguments flipped.
-- |
-- | This can be useful when running an action written using do notation
-- | for every element in a data structure:
-- |
-- | For example:
-- |
-- | ```purescript
-- | ifor_ ["a", "b", "c"] \i x -> do
-- |   logShow i
-- |   log x
-- | ```
ifor_
  :: forall i a b f m
   . Applicative m
  => FoldableWithIndex i f
  => f a
  -> (i -> a -> m b)
  -> m Unit
ifor_ = flip itraverse_

-- | `ifoldMap` but with each element surrounded by some fixed value.
-- |
-- | For example:
-- |
-- | ```purescript
-- | > isurroundMap "*" (\i x -> show i <> x) []
-- | = "*"
-- |
-- | > isurroundMap "*" (\i x -> show i <> x) ["a"]
-- | = "*0a*"
-- |
-- | > isurroundMap "*" (\i x -> show i <> x) ["a", "b"]
-- | = "*0a*1b*"
-- |
-- | > isurroundMap "*" (\i x -> show i <> x) ["a", "b", "c"]
-- | = "*0a*1b*2c*"
-- | ```
isurroundMap
  :: forall i f a m
   . FoldableWithIndex i f
  => Semigroup m
  => m
  -> (i -> a -> m)
  -> f a
  -> m
isurroundMap d t f = unwrap (ifoldMap joined f) d
  where joined i a = Endo \m -> d <> t i a <> m

-- | `iall f` is the same as `and <<< imap f`; map a function over the
-- | structure, and then get the conjunction of the results.
iall
  :: forall i a b f
   . FoldableWithIndex i f
  => HeytingAlgebra b
  => (i -> a -> b)
  -> f a
  -> b
iall t = unwrap <<< ifoldMap (\i -> Conj <<< t i)

-- | `iany f` is the same as `or <<< imap f`; map a function over the
-- | structure, and then get the disjunction of the results.
iany
  :: forall i a b f
   . FoldableWithIndex i f
  => HeytingAlgebra b
  => (i -> a -> b)
  -> f a
  -> b
iany t = unwrap <<< ifoldMap (\i -> Disj <<< t i)

-- | Try to find an element in a data structure which satisfies a predicate
-- | with access to the index.
ifind
  :: forall i a f
   . FoldableWithIndex i f
  => (i -> a -> Boolean)
  -> f a
  -> Maybe a
ifind p = ifoldl go Nothing
  where
  go i Nothing x | p i x = Just x
  go i r _ = r
