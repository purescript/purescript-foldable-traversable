module Data.Bitraversable
  ( class Bitraversable, bitraverse, bisequence
  , bitraverseDefault
  , bisequenceDefault
  , ltraverse
  , rtraverse
  , bifor
  , lfor
  , rfor
  , module Data.Bifoldable
  ) where

import Prelude

import Data.Bifoldable (class Bifoldable, biall, biany, bifold, bifoldMap, bifoldMapDefaultL, bifoldMapDefaultR, bifoldl, bifoldlDefault, bifoldr, bifoldrDefault, bifor_, bisequence_, bitraverse_)
import Data.Bifunctor (class Bifunctor, bimap)

-- | `Bitraversable` represents data structures with two type arguments which can be
-- | traversed.
-- |
-- | A traversal for such a structure requires two functions, one for each type
-- | argument. Type class instances should choose the appropriate function based
-- | on the type of the element encountered at each point of the traversal.
-- |
-- | Default implementations are provided by the following functions:
-- |
-- | - `bitraverseDefault`
-- | - `bisequenceDefault`
class (Bifunctor t, Bifoldable t) <= Bitraversable t where
  bitraverse :: forall f a b c d. Applicative f => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
  bisequence :: forall f a b. Applicative f => t (f a) (f b) -> f (t a b)

ltraverse
  :: forall t b c a f
   . (Bitraversable t, Applicative f)
  => (a -> f c)
  -> t a b
  -> f (t c b)
ltraverse f = bitraverse f pure

rtraverse
  :: forall t b c a f
   . (Bitraversable t, Applicative f)
  => (b -> f c)
  -> t a b
  -> f (t a c)
rtraverse = bitraverse pure

-- | A default implementation of `bitraverse` using `bisequence` and `bimap`.
bitraverseDefault
  :: forall t f a b c d
   . (Bitraversable t, Applicative f)
  => (a -> f c)
  -> (b -> f d)
  -> t a b
  -> f (t c d)
bitraverseDefault f g t = bisequence (bimap f g t)

-- | A default implementation of `bisequence` using `bitraverse`.
bisequenceDefault
  :: forall t f a b
   . (Bitraversable t, Applicative f)
  => t (f a) (f b)
  -> f (t a b)
bisequenceDefault t = bitraverse id id t

-- | Traverse a data structure, accumulating effects and results using an `Applicative` functor.
bifor
  :: forall t f a b c d
   . (Bitraversable t, Applicative f)
  => t a b
  -> (a -> f c)
  -> (b -> f d)
  -> f (t c d)
bifor t f g = bitraverse f g t

lfor
  :: forall t b c a f
   . (Bitraversable t, Applicative f)
  => t a b
  -> (a -> f c)
  -> f (t c b)
lfor t f = bitraverse f pure t

rfor
  :: forall t b c a f
   . (Bitraversable t, Applicative f)
  => t a b
  -> (b -> f c)
  -> f (t a c)
rfor t f = bitraverse pure f t
