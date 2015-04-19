module Data.Bitraversable where

import Data.Bifoldable
import Data.Bifunctor (Bifunctor)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))

-- | `Bitraversable` represents data structures with two type arguments which can be
-- | traversed.
-- |
-- | A traversal for such a structure requires two functions, one for each type
-- | argument. Type class instances should choose the appropriate function based
-- | on the type of the element encountered at each point of the traversal.
-- |
class (Bifunctor t, Bifoldable t) <= Bitraversable t where
  bitraverse :: forall f a b c d. (Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
  bisequence :: forall f a b. (Applicative f) => t (f a) (f b) -> f (t a b)

instance bitraversableTuple :: Bitraversable Tuple where
  bitraverse f g (Tuple a b) = Tuple <$> f a <*> g b
  bisequence (Tuple a b) = Tuple <$> a <*> b

instance bitraversableEither :: Bitraversable Either where
  bitraverse f _ (Left a) = Left <$> f a
  bitraverse _ g (Right b) = Right <$> g b
  bisequence (Left a) = Left <$> a
  bisequence (Right b) = Right <$> b

-- | Traverse a data structure, accumulating effects and results using an `Applicative` functor.
bifor :: forall t f a b c d. (Bitraversable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f (t c d)
bifor t f g = bitraverse f g t
