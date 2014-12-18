# Module Documentation

## Module Data.Foldable

### Type Classes

    class Foldable f where
      foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
      foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
      foldMap :: forall a m. (Monoid m) => (a -> m) -> f a -> m


### Type Class Instances

    instance foldableArray :: Foldable Prim.Array

    instance foldableEither :: Foldable (Either a)

    instance foldableMaybe :: Foldable Maybe

    instance foldableTuple :: Foldable (Tuple a)


### Values

    all :: forall a f. (Foldable f) => (a -> Boolean) -> f a -> Boolean

    and :: forall f. (Foldable f) => f Boolean -> Boolean

    any :: forall a f. (Foldable f) => (a -> Boolean) -> f a -> Boolean

    elem :: forall a f. (Eq a, Foldable f) => a -> f a -> Boolean

    find :: forall a f. (Foldable f) => (a -> Boolean) -> f a -> Maybe a

    fold :: forall f m. (Foldable f, Monoid m) => f m -> m

    foldlArray :: forall a b. (b -> a -> b) -> b -> [a] -> b

    foldrArray :: forall a b. (a -> b -> b) -> b -> [a] -> b

    for_ :: forall a b f m. (Applicative m, Foldable f) => f a -> (a -> m b) -> m Unit

    intercalate :: forall f m. (Foldable f, Monoid m) => m -> f m -> m

    lookup :: forall a b f. (Eq a, Foldable f) => a -> f (Tuple a b) -> Maybe b

    mconcat :: forall f m. (Foldable f, Monoid m) => f m -> m

    notElem :: forall a f. (Eq a, Foldable f) => a -> f a -> Boolean

    or :: forall f. (Foldable f) => f Boolean -> Boolean

    product :: forall f. (Foldable f) => f Number -> Number

    sequence_ :: forall a f m. (Applicative m, Foldable f) => f (m a) -> m Unit

    sum :: forall f. (Foldable f) => f Number -> Number

    traverse_ :: forall a b f m. (Applicative m, Foldable f) => (a -> m b) -> f a -> m Unit


## Module Data.Traversable

### Types


### Type Classes

    class (Functor t, Foldable t) <= Traversable t where
      traverse :: forall a b m. (Applicative m) => (a -> m b) -> t a -> m (t b)
      sequence :: forall a m. (Applicative m) => t (m a) -> m (t a)


### Type Class Instances

    instance applicativeStateL :: Applicative (StateL s)

    instance applicativeStateR :: Applicative (StateR s)

    instance applyStateL :: Apply (StateL s)

    instance applyStateR :: Apply (StateR s)

    instance functorStateL :: Functor (StateL s)

    instance functorStateR :: Functor (StateR s)

    instance traversableArray :: Traversable Prim.Array

    instance traversableEither :: Traversable (Either a)

    instance traversableMaybe :: Traversable Maybe

    instance traversableTuple :: Traversable (Tuple a)


### Values

    for :: forall a b m t. (Applicative m, Traversable t) => t a -> (a -> m b) -> m (t b)

    mapAccumL :: forall a b s f. (Traversable f) => (s -> a -> Tuple s b) -> s -> f a -> Tuple s (f b)

    mapAccumR :: forall a b s f. (Traversable f) => (s -> a -> Tuple s b) -> s -> f a -> Tuple s (f b)

    scanl :: forall a b f. (Traversable f) => (b -> a -> b) -> b -> f a -> f b

    scanr :: forall a b f. (Traversable f) => (a -> b -> b) -> b -> f a -> f b

    zipWithA :: forall m a b c. (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m [c]