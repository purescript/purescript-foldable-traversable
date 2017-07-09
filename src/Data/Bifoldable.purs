module Data.Bifoldable where

import Prelude

import Control.Apply (applySecond)

import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Monoid.Dual (Dual(..))
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Bifunctor.Clown (Clown(..))
import Data.Bifunctor.Joker (Joker(..))
import Data.Bifunctor.Flip (Flip(..))
import Data.Bifunctor.Product (Product(..))
import Data.Bifunctor.Wrap (Wrap(..))

-- | `Bifoldable` represents data structures with two type arguments which can be
-- | folded.
-- |
-- | A fold for such a structure requires two step functions, one for each type
-- | argument. Type class instances should choose the appropriate step function based
-- | on the type of the element encountered at each point of the fold.
-- |
-- | Default implementations are provided by the following functions:
-- |
-- | - `bfoldrWithIndexDefault`
-- | - `bfoldlWithIndexDefault`
-- | - `bfoldMapWithIndexDefaultR`
-- | - `bfoldMapWithIndexDefaultL`
-- |
-- | Note: some combinations of the default implementations are unsafe to
-- | use together - causing a non-terminating mutually recursive cycle.
-- | These combinations are documented per function.
class Bifoldable p where
  bfoldrWithIndex :: forall a b c. (a -> c -> c) -> (b -> c -> c) -> c -> p a b -> c
  bfoldlWithIndex :: forall a b c. (c -> a -> c) -> (c -> b -> c) -> c -> p a b -> c
  bfoldMapWithIndex :: forall m a b. Monoid m => (a -> m) -> (b -> m) -> p a b -> m

instance bifoldableClown :: Foldable f => Bifoldable (Clown f) where
  bfoldrWithIndex l _ u (Clown f) = foldr l u f
  bfoldlWithIndex l _ u (Clown f) = foldl l u f
  bfoldMapWithIndex l _ (Clown f) = foldMap l f

instance bifoldableJoker :: Foldable f => Bifoldable (Joker f) where
  bfoldrWithIndex _ r u (Joker f) = foldr r u f
  bfoldlWithIndex _ r u (Joker f) = foldl r u f
  bfoldMapWithIndex _ r (Joker f) = foldMap r f

instance bifoldableFlip :: Bifoldable p => Bifoldable (Flip p) where
  bfoldrWithIndex r l u (Flip p) = bfoldrWithIndex l r u p
  bfoldlWithIndex r l u (Flip p) = bfoldlWithIndex l r u p
  bfoldMapWithIndex r l (Flip p) = bfoldMapWithIndex l r p

instance bifoldableProduct :: (Bifoldable f, Bifoldable g) => Bifoldable (Product f g) where
  bfoldrWithIndex l r u m = bfoldrWithIndexDefault l r u m
  bfoldlWithIndex l r u m = bfoldlWithIndexDefault l r u m
  bfoldMapWithIndex l r (Product f g) = bfoldMapWithIndex l r f <> bfoldMapWithIndex l r g

instance bifoldableWrap :: Bifoldable p => Bifoldable (Wrap p) where
  bfoldrWithIndex l r u (Wrap p) = bfoldrWithIndex l r u p
  bfoldlWithIndex l r u (Wrap p) = bfoldlWithIndex l r u p
  bfoldMapWithIndex l r (Wrap p) = bfoldMapWithIndex l r p

-- | A default implementation of `bfoldrWithIndex` using `bfoldMapWithIndex`.
-- |
-- | Note: when defining a `Bifoldable` instance, this function is unsafe to
-- | use in combination with `bfoldMapWithIndexDefaultR`.
bfoldrWithIndexDefault
  :: forall p a b c
   . Bifoldable p
  => (a -> c -> c)
  -> (b -> c -> c)
  -> c
  -> p a b
  -> c
bfoldrWithIndexDefault f g z p = unwrap (bfoldMapWithIndex (Endo <<< f) (Endo <<< g) p) z

-- | A default implementation of `bfoldlWithIndex` using `bfoldMapWithIndex`.
-- |
-- | Note: when defining a `Bifoldable` instance, this function is unsafe to
-- | use in combination with `bfoldMapWithIndexDefaultL`.
bfoldlWithIndexDefault
  :: forall p a b c
   . Bifoldable p
  => (c -> a -> c)
  -> (c -> b -> c)
  -> c
  -> p a b
  -> c
bfoldlWithIndexDefault f g z p =
  unwrap
    (unwrap
      (bfoldMapWithIndex (Dual <<< Endo <<< flip f) (Dual <<< Endo <<< flip g) p))
    z

-- | A default implementation of `bfoldMapWithIndex` using `bfoldrWithIndex`.
-- |
-- | Note: when defining a `Bifoldable` instance, this function is unsafe to
-- | use in combination with `bfoldrWithIndexDefault`.
bfoldMapWithIndexDefaultR
  :: forall p m a b
   . Bifoldable p
  => Monoid m
  => (a -> m)
  -> (b -> m)
  -> p a b
  -> m
bfoldMapWithIndexDefaultR f g = bfoldrWithIndex (append <<< f) (append <<< g) mempty

-- | A default implementation of `bfoldMapWithIndex` using `bfoldlWithIndex`.
-- |
-- | Note: when defining a `Bifoldable` instance, this function is unsafe to
-- | use in combination with `bfoldlWithIndexDefault`.
bfoldMapWithIndexDefaultL
  :: forall p m a b
   . Bifoldable p
  => Monoid m
  => (a -> m)
  -> (b -> m)
  -> p a b
  -> m
bfoldMapWithIndexDefaultL f g = bfoldlWithIndex (\m a -> m <> f a) (\m b -> m <> g b) mempty


-- | Fold a data structure, accumulating values in a monoidal type.
bifold :: forall t m. Bifoldable t => Monoid m => t m m -> m
bifold = bfoldMapWithIndex id id

-- | Traverse a data structure, accumulating effects using an `Applicative` functor,
-- | ignoring the final result.
btraverseWithIndex_
  :: forall t f a b c d
   . Bifoldable t
  => Applicative f
  => (a -> f c)
  -> (b -> f d)
  -> t a b
  -> f Unit
btraverseWithIndex_ f g = bfoldrWithIndex (applySecond <<< f) (applySecond <<< g) (pure unit)

-- | A version of `btraverseWithIndex_` with the data structure as the first argument.
bforWithIndex_
  :: forall t f a b c d
   . Bifoldable t
  => Applicative f
  => t a b
  -> (a -> f c)
  -> (b -> f d)
  -> f Unit
bforWithIndex_ t f g = btraverseWithIndex_ f g t

-- | Collapse a data structure, collecting effects using an `Applicative` functor,
-- | ignoring the final result.
bisequence_
  :: forall t f a b
   . Bifoldable t
  => Applicative f
  => t (f a) (f b)
  -> f Unit
bisequence_ = btraverseWithIndex_ id id

-- | Test whether a predicate holds at any position in a data structure.
banyWithIndex
  :: forall t a b c
   . Bifoldable t
  => BooleanAlgebra c
  => (a -> c)
  -> (b -> c)
  -> t a b
  -> c
banyWithIndex p q = unwrap <<< bfoldMapWithIndex (Disj <<< p) (Disj <<< q)

-- | Test whether a predicate holds at all positions in a data structure.
ballWithIndex
  :: forall t a b c
   . Bifoldable t
  => BooleanAlgebra c
  => (a -> c)
  -> (b -> c)
  -> t a b
  -> c
ballWithIndex p q = unwrap <<< bfoldMapWithIndex (Conj <<< p) (Conj <<< q)
