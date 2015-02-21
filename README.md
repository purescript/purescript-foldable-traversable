# Module Documentation

## Module Data.Foldable

#### `Foldable`

``` purescript
class Foldable f where
  foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldMap :: forall a m. (Monoid m) => (a -> m) -> f a -> m
```


#### `foldableArray`

``` purescript
instance foldableArray :: Foldable Prim.Array
```


#### `foldableEither`

``` purescript
instance foldableEither :: Foldable (Either a)
```


#### `foldableMaybe`

``` purescript
instance foldableMaybe :: Foldable Maybe
```


#### `foldableTuple`

``` purescript
instance foldableTuple :: Foldable (Tuple a)
```


#### `foldableAdditive`

``` purescript
instance foldableAdditive :: Foldable Additive
```


#### `foldableDual`

``` purescript
instance foldableDual :: Foldable Dual
```


#### `foldableFirst`

``` purescript
instance foldableFirst :: Foldable First
```


#### `foldableLast`

``` purescript
instance foldableLast :: Foldable Last
```


#### `foldableMultiplicative`

``` purescript
instance foldableMultiplicative :: Foldable Multiplicative
```


#### `fold`

``` purescript
fold :: forall f m. (Foldable f, Monoid m) => f m -> m
```


#### `traverse_`

``` purescript
traverse_ :: forall a b f m. (Applicative m, Foldable f) => (a -> m b) -> f a -> m Unit
```


#### `for_`

``` purescript
for_ :: forall a b f m. (Applicative m, Foldable f) => f a -> (a -> m b) -> m Unit
```


#### `sequence_`

``` purescript
sequence_ :: forall a f m. (Applicative m, Foldable f) => f (m a) -> m Unit
```


#### `mconcat`

``` purescript
mconcat :: forall f m. (Foldable f, Monoid m) => f m -> m
```


#### `intercalate`

``` purescript
intercalate :: forall f m. (Foldable f, Monoid m) => m -> f m -> m
```


#### `and`

``` purescript
and :: forall f. (Foldable f) => f Boolean -> Boolean
```


#### `or`

``` purescript
or :: forall f. (Foldable f) => f Boolean -> Boolean
```


#### `any`

``` purescript
any :: forall a f. (Foldable f) => (a -> Boolean) -> f a -> Boolean
```


#### `all`

``` purescript
all :: forall a f. (Foldable f) => (a -> Boolean) -> f a -> Boolean
```


#### `sum`

``` purescript
sum :: forall f. (Foldable f) => f Number -> Number
```


#### `product`

``` purescript
product :: forall f. (Foldable f) => f Number -> Number
```


#### `elem`

``` purescript
elem :: forall a f. (Eq a, Foldable f) => a -> f a -> Boolean
```


#### `notElem`

``` purescript
notElem :: forall a f. (Eq a, Foldable f) => a -> f a -> Boolean
```


#### `find`

``` purescript
find :: forall a f. (Foldable f) => (a -> Boolean) -> f a -> Maybe a
```


#### `lookup`

``` purescript
lookup :: forall a b f. (Eq a, Foldable f) => a -> f (Tuple a b) -> Maybe b
```


#### `foldrArray`

``` purescript
foldrArray :: forall a b. (a -> b -> b) -> b -> [a] -> b
```


#### `foldlArray`

``` purescript
foldlArray :: forall a b. (b -> a -> b) -> b -> [a] -> b
```



## Module Data.Traversable

#### `Traversable`

``` purescript
class (Functor t, Foldable t) <= Traversable t where
  traverse :: forall a b m. (Applicative m) => (a -> m b) -> t a -> m (t b)
  sequence :: forall a m. (Applicative m) => t (m a) -> m (t a)
```


#### `traversableArray`

``` purescript
instance traversableArray :: Traversable Prim.Array
```


#### `traversableEither`

``` purescript
instance traversableEither :: Traversable (Either a)
```


#### `traversableMaybe`

``` purescript
instance traversableMaybe :: Traversable Maybe
```


#### `traversableTuple`

``` purescript
instance traversableTuple :: Traversable (Tuple a)
```


#### `traversableAdditive`

``` purescript
instance traversableAdditive :: Traversable Additive
```


#### `traversableDual`

``` purescript
instance traversableDual :: Traversable Dual
```


#### `traversableFirst`

``` purescript
instance traversableFirst :: Traversable First
```


#### `traversableLast`

``` purescript
instance traversableLast :: Traversable Last
```


#### `traversableMultiplicative`

``` purescript
instance traversableMultiplicative :: Traversable Multiplicative
```


#### `for`

``` purescript
for :: forall a b m t. (Applicative m, Traversable t) => t a -> (a -> m b) -> m (t b)
```


#### `zipWithA`

``` purescript
zipWithA :: forall m a b c. (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
```


#### `functorStateL`

``` purescript
instance functorStateL :: Functor (StateL s)
```


#### `applyStateL`

``` purescript
instance applyStateL :: Apply (StateL s)
```


#### `applicativeStateL`

``` purescript
instance applicativeStateL :: Applicative (StateL s)
```


#### `scanl`

``` purescript
scanl :: forall a b f. (Traversable f) => (b -> a -> b) -> b -> f a -> f b
```


#### `mapAccumL`

``` purescript
mapAccumL :: forall a b s f. (Traversable f) => (s -> a -> Tuple s b) -> s -> f a -> Tuple s (f b)
```


#### `functorStateR`

``` purescript
instance functorStateR :: Functor (StateR s)
```


#### `applyStateR`

``` purescript
instance applyStateR :: Apply (StateR s)
```


#### `applicativeStateR`

``` purescript
instance applicativeStateR :: Applicative (StateR s)
```


#### `scanr`

``` purescript
scanr :: forall a b f. (Traversable f) => (a -> b -> b) -> b -> f a -> f b
```


#### `mapAccumR`

``` purescript
mapAccumR :: forall a b s f. (Traversable f) => (s -> a -> Tuple s b) -> s -> f a -> Tuple s (f b)
```