module Data.Bitraversable
  ( Bitraversable, bitraverse, bisequence
  , bitraverseDefault, bisequenceDefault
  , bifoldMapDefaultT, bimapDefault
  , bifor
  ) where

import Prelude

import Data.Monoid (Monoid, mempty)
import Data.Bifoldable
import Data.Bifunctor (Bifunctor, bimap)

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
  bitraverse :: forall f a b c d. (Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
  bisequence :: forall f a b. (Applicative f) => t (f a) (f b) -> f (t a b)


-- | A default implementation of `bitraverse` using `bisequence` and `bimap`.
bitraverseDefault :: forall t f a b c d. (Bitraversable t, Applicative f) =>
                     (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
bitraverseDefault f g t = bisequence (bimap f g t)

-- | A default implementation of `bisequence` using `bitraverse`.
bisequenceDefault :: forall t f a b. (Bitraversable t, Applicative f) =>
                     t (f a) (f b) -> f (t a b)
bisequenceDefault t = bitraverse id id t


newtype Id a = Id a

runId :: forall a. Id a -> a
runId (Id a) = a

instance functorId     :: Functor Id     where map f (Id a) = Id (f a)
instance applyId       :: Apply Id       where apply (Id f) (Id a) = Id (f a)
instance applicativeId :: Applicative Id where pure = Id

-- | A default implementation of `bimap` using `bitraverse`.
-- | Note: it is unsafe to use both `bimapDefault` and `bitraverseDefault`.
bimapDefault :: forall t a b c d. (Bitraversable t) =>
                (a -> c) -> (b -> d) -> t a b -> t c d
bimapDefault f g m = runId $ bitraverse (Id <<< f) (Id <<< g) m


newtype Const a b = Const a

runConst :: forall a b. Const a b -> a
runConst (Const a) = a

instance functorConst :: Functor (Const m) where
  map f (Const a) = Const a

instance applyConst :: (Monoid m) => Apply (Const m) where
  apply (Const l) (Const r) = Const (l <> r)

instance applicativeConst :: (Monoid m) => Applicative (Const m) where
  pure _ = Const mempty

-- | A default implementation of `bifoldMap` using `bitraverse`.
bifoldMapDefaultT :: forall t a b m. (Bitraversable t, Monoid m) => (a -> m) -> (b -> m) -> t a b -> m
bifoldMapDefaultT f g t = runConst $ bitraverse (Const <<< f) (Const <<< g) t


-- | Traverse a data structure, accumulating effects and results using an `Applicative` functor.
bifor :: forall t f a b c d. (Bitraversable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f (t c d)
bifor t f g = bitraverse f g t
