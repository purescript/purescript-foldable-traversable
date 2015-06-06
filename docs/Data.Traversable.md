## Module Data.Traversable

#### `Traversable`

``` purescript
class (Functor t, Foldable t) <= Traversable t where
  traverse :: forall a b m. (Applicative m) => (a -> m b) -> t a -> m (t b)
  sequence :: forall a m. (Applicative m) => t (m a) -> m (t a)
```

`Traversable` represents data structures which can be _traversed_,
accumulating results and effects in some `Applicative` functor.

- `traverse` runs an action for every element in a data structure,
  and accumulates the results.
- `sequence` runs the actions _contained_ in a data structure,
  and accumulates the results.

The `traverse` and `sequence` functions should be compatible in the
following sense:

- `traverse f xs = sequence (f <$> xs)`
- `sequence = traverse id`

`Traversable` instances should also be compatible with the corresponding
`Foldable` instances, in the following sense:

- `foldMap f = runConst <<< traverse (Const <<< f)`

##### Instances
``` purescript
instance traversableArray :: Traversable Array
instance traversableMaybe :: Traversable Maybe
instance traversableFirst :: Traversable First
instance traversableLast :: Traversable Last
instance traversableAdditive :: Traversable Additive
instance traversableDual :: Traversable Dual
instance traversableConj :: Traversable Conj
instance traversableDisj :: Traversable Disj
instance traversableMultiplicative :: Traversable Multiplicative
```

#### `for`

``` purescript
for :: forall a b m t. (Applicative m, Traversable t) => t a -> (a -> m b) -> m (t b)
```

A version of `traverse` with its arguments flipped.


This can be useful when running an action written using do notation
for every element in a data structure:

For example:

```purescript
for [1, 2, 3] \n -> do
  print n
  return (n * n)
```

#### `Accum`

``` purescript
type Accum s a = { accum :: s, value :: a }
```

#### `scanl`

``` purescript
scanl :: forall a b f. (Traversable f) => (b -> a -> b) -> b -> f a -> f b
```

Fold a data structure from the left, keeping all intermediate results
instead of only the final result.

#### `mapAccumL`

``` purescript
mapAccumL :: forall a b s f. (Traversable f) => (s -> a -> Accum s b) -> s -> f a -> Accum s (f b)
```

Fold a data structure from the left, keeping all intermediate results
instead of only the final result.

Unlike `scanl`, `mapAccumL` allows the type of accumulator to differ
from the element type of the final data structure.

#### `scanr`

``` purescript
scanr :: forall a b f. (Traversable f) => (a -> b -> b) -> b -> f a -> f b
```

Fold a data structure from the right, keeping all intermediate results
instead of only the final result.

#### `mapAccumR`

``` purescript
mapAccumR :: forall a b s f. (Traversable f) => (s -> a -> Accum s b) -> s -> f a -> Accum s (f b)
```

Fold a data structure from the right, keeping all intermediate results
instead of only the final result.

Unlike `scanr`, `mapAccumR` allows the type of accumulator to differ
from the element type of the final data structure.


