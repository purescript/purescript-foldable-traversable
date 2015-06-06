module Data.Bifoldable where

import Prelude

import Control.Apply ((*>))
import Data.Monoid (Monoid)
import Data.Monoid.Disj (Disj(..), runDisj)
import Data.Monoid.Conj (Conj(..), runConj)

-- | `Bifoldable` represents data structures with two type arguments which can be
-- | folded.
-- |
-- | A fold for such a structure requires two step functions, one for each type
-- | argument. Type class instances should choose the appropriate step function based
-- | on the type of the element encountered at each point of the fold.
-- |
class Bifoldable p where
  bifoldr :: forall a b c. (a -> c -> c) -> (b -> c -> c) -> c -> p a b -> c
  bifoldl :: forall a b c. (c -> a -> c) -> (c -> b -> c) -> c -> p a b -> c
  bifoldMap :: forall m a b. (Monoid m) => (a -> m) -> (b -> m) -> p a b -> m

-- | Fold a data structure, accumulating values in a monoidal type.
bifold :: forall t m. (Bifoldable t, Monoid m) => t m m -> m
bifold = bifoldMap id id

-- | Traverse a data structure, accumulating effects using an `Applicative` functor,
-- | ignoring the final result.
bitraverse_ :: forall t f a b c d. (Bifoldable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f Unit
bitraverse_ f g = bifoldr ((*>) <<< f) ((*>) <<< g) (pure unit)

-- | A version of `bitraverse_` with the data structure as the first argument.
bifor_ :: forall t f a b c d. (Bifoldable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f Unit
bifor_ t f g = bitraverse_ f g t

-- | Collapse a data structure, collecting effects using an `Applicative` functor,
-- | ignoring the final result.
bisequence_ :: forall t f a b. (Bifoldable t, Applicative f) => t (f a) (f b) -> f Unit
bisequence_ = bitraverse_ id id

-- | Test whether a predicate holds at any position in a data structure.
biany :: forall t a b c. (Bifoldable t, BooleanAlgebra c) => (a -> c) -> (b -> c) -> t a b -> c
biany p q = runDisj <<< bifoldMap (Disj <<< p) (Disj <<< q)

-- -- | Test whether a predicate holds at all positions in a data structure.
biall :: forall t a b c. (Bifoldable t, BooleanAlgebra c) => (a -> c) -> (b -> c) -> t a b -> c
biall p q = runConj <<< bifoldMap (Conj <<< p) (Conj <<< q)
