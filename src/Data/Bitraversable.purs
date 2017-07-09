module Data.Bitraversable
  ( class Bitraversable, btraverseWithIndex, bisequence
  , btraverseWithIndexDefault
  , bisequenceDefault
  , ltraverse
  , rtraverse
  , bforWithIndex
  , lfor
  , rfor
  , module Data.Bifoldable
  ) where

import Prelude

import Data.Bifoldable (class Bifoldable, ballWithIndex, banyWithIndex, bifold, bfoldMapWithIndex, bfoldMapWithIndexDefaultL, bfoldMapWithIndexDefaultR, bfoldlWithIndex, bfoldlWithIndexDefault, bfoldrWithIndex, bfoldrWithIndexDefault, bforWithIndex_, bisequence_, btraverseWithIndex_)
import Data.Traversable (class Traversable, traverse, sequence)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Bifunctor.Clown (Clown(..))
import Data.Bifunctor.Joker (Joker(..))
import Data.Bifunctor.Flip (Flip(..))
import Data.Bifunctor.Product (Product(..))
import Data.Bifunctor.Wrap (Wrap(..))

-- | `Bitraversable` represents data structures with two type arguments which can be
-- | traversed.
-- |
-- | A traversal for such a structure requires two functions, one for each type
-- | argument. Type class instances should choose the appropriate function based
-- | on the type of the element encountered at each point of the traversal.
-- |
-- | Default implementations are provided by the following functions:
-- |
-- | - `btraverseWithIndexDefault`
-- | - `bisequenceDefault`
class (Bifunctor t, Bifoldable t) <= Bitraversable t where
  btraverseWithIndex :: forall f a b c d. Applicative f => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
  bisequence :: forall f a b. Applicative f => t (f a) (f b) -> f (t a b)

instance bitraversableClown :: Traversable f => Bitraversable (Clown f) where
  btraverseWithIndex l _ (Clown f) = Clown <$> traverse l f
  bisequence (Clown f) = Clown <$> sequence f

instance bitraversableJoker :: Traversable f => Bitraversable (Joker f) where
  btraverseWithIndex _ r (Joker f) = Joker <$> traverse r f
  bisequence (Joker f) = Joker <$> sequence f

instance bitraversableFlip :: Bitraversable p => Bitraversable (Flip p) where
  btraverseWithIndex r l (Flip p) = Flip <$> btraverseWithIndex l r p
  bisequence (Flip p) = Flip <$> bisequence p

instance bitraversableProduct :: (Bitraversable f, Bitraversable g) => Bitraversable (Product f g) where
  btraverseWithIndex l r (Product f g) = Product <$> btraverseWithIndex l r f <*> btraverseWithIndex l r g
  bisequence (Product f g) = Product <$> bisequence f <*> bisequence g

instance bitraversableWrap :: Bitraversable p => Bitraversable (Wrap p) where
  btraverseWithIndex l r (Wrap p) = Wrap <$> btraverseWithIndex l r p
  bisequence (Wrap p) = Wrap <$> bisequence p

ltraverse
  :: forall t b c a f
   . Bitraversable t
  => Applicative f
  => (a -> f c)
  -> t a b
  -> f (t c b)
ltraverse f = btraverseWithIndex f pure

rtraverse
  :: forall t b c a f
   . Bitraversable t
  => Applicative f
  => (b -> f c)
  -> t a b
  -> f (t a c)
rtraverse = btraverseWithIndex pure

-- | A default implementation of `btraverseWithIndex` using `bisequence` and `bimap`.
btraverseWithIndexDefault
  :: forall t f a b c d
   . Bitraversable t
  => Applicative f
  => (a -> f c)
  -> (b -> f d)
  -> t a b
  -> f (t c d)
btraverseWithIndexDefault f g t = bisequence (bimap f g t)

-- | A default implementation of `bisequence` using `btraverseWithIndex`.
bisequenceDefault
  :: forall t f a b
   . Bitraversable t
  => Applicative f
  => t (f a) (f b)
  -> f (t a b)
bisequenceDefault = btraverseWithIndex id id

-- | Traverse a data structure, accumulating effects and results using an `Applicative` functor.
bforWithIndex
  :: forall t f a b c d
   . Bitraversable t
  => Applicative f
  => t a b
  -> (a -> f c)
  -> (b -> f d)
  -> f (t c d)
bforWithIndex t f g = btraverseWithIndex f g t

lfor
  :: forall t b c a f
   . Bitraversable t
  => Applicative f
  => t a b
  -> (a -> f c)
  -> f (t c b)
lfor t f = btraverseWithIndex f pure t

rfor
  :: forall t b c a f
   . Bitraversable t
  => Applicative f
  => t a b
  -> (b -> f c)
  -> f (t a c)
rfor t f = btraverseWithIndex pure f t
