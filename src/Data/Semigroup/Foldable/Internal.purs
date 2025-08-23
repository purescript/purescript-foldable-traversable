module Data.Semigroup.Foldable.Internal where

import Prelude

newtype Act :: forall k. (k -> Type) -> k -> Type
newtype Act f a = Act (f a)

getAct :: forall f a. Act f a -> f a
getAct (Act f) = f

instance semigroupAct :: Apply f => Semigroup (Act f a) where
  append (Act a) (Act b) = Act (a *> b)

-- | Internal. Used by intercalation functions.
newtype JoinWith a = JoinWith (a -> a)

joinee :: forall a. JoinWith a -> a -> a
joinee (JoinWith x) = x

instance semigroupJoinWith :: Semigroup a => Semigroup (JoinWith a) where
  append (JoinWith a) (JoinWith b) = JoinWith $ \j -> a j <> j <> b j
