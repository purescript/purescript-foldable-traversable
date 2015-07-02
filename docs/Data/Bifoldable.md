## Module Data.Bifoldable

#### `Bifoldable`

``` purescript
class Bifoldable p where
  bifoldr :: forall a b c. (a -> c -> c) -> (b -> c -> c) -> c -> p a b -> c
  bifoldl :: forall a b c. (c -> a -> c) -> (c -> b -> c) -> c -> p a b -> c
  bifoldMap :: forall m a b. (Monoid m) => (a -> m) -> (b -> m) -> p a b -> m
```

`Bifoldable` represents data structures with two type arguments which can be
folded.

A fold for such a structure requires two step functions, one for each type
argument. Type class instances should choose the appropriate step function based
on the type of the element encountered at each point of the fold.

Default implementations are provided by the following functions:

- `bifoldrDefault`
- `bifoldlDefault`
- `bifoldMapDefaultR`
- `bifoldMapDefaultL`

Note: some combinations of the default implementations are unsafe to
use together - causing a non-terminating mutually recursive cycle.
These combinations are documented per function.

#### `bifoldrDefault`

``` purescript
bifoldrDefault :: forall p a b c. (Bifoldable p) => (a -> c -> c) -> (b -> c -> c) -> c -> p a b -> c
```

A default implementation of `bifoldr` using `bifoldMap`.

Note: when defining a `Bifoldable` instance, this function is unsafe to
use in combination with `bifoldMapDefaultR`.

#### `bifoldlDefault`

``` purescript
bifoldlDefault :: forall p a b c. (Bifoldable p) => (c -> a -> c) -> (c -> b -> c) -> c -> p a b -> c
```

A default implementation of `bifoldl` using `bifoldMap`.

Note: when defining a `Bifoldable` instance, this function is unsafe to
use in combination with `bifoldMapDefaultL`.

#### `bifoldMapDefaultR`

``` purescript
bifoldMapDefaultR :: forall p m a b. (Bifoldable p, Monoid m) => (a -> m) -> (b -> m) -> p a b -> m
```

A default implementation of `bifoldMap` using `bifoldr`.

Note: when defining a `Bifoldable` instance, this function is unsafe to
use in combination with `bifoldrDefault`.

#### `bifoldMapDefaultL`

``` purescript
bifoldMapDefaultL :: forall p m a b. (Bifoldable p, Monoid m) => (a -> m) -> (b -> m) -> p a b -> m
```

A default implementation of `bifoldMap` using `bifoldl`.

Note: when defining a `Bifoldable` instance, this function is unsafe to
use in combination with `bifoldlDefault`.

#### `bifold`

``` purescript
bifold :: forall t m. (Bifoldable t, Monoid m) => t m m -> m
```

Fold a data structure, accumulating values in a monoidal type.

#### `bitraverse_`

``` purescript
bitraverse_ :: forall t f a b c d. (Bifoldable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f Unit
```

Traverse a data structure, accumulating effects using an `Applicative` functor,
ignoring the final result.

#### `bifor_`

``` purescript
bifor_ :: forall t f a b c d. (Bifoldable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f Unit
```

A version of `bitraverse_` with the data structure as the first argument.

#### `bisequence_`

``` purescript
bisequence_ :: forall t f a b. (Bifoldable t, Applicative f) => t (f a) (f b) -> f Unit
```

Collapse a data structure, collecting effects using an `Applicative` functor,
ignoring the final result.

#### `biany`

``` purescript
biany :: forall t a b c. (Bifoldable t, BooleanAlgebra c) => (a -> c) -> (b -> c) -> t a b -> c
```

Test whether a predicate holds at any position in a data structure.

#### `biall`

``` purescript
biall :: forall t a b c. (Bifoldable t, BooleanAlgebra c) => (a -> c) -> (b -> c) -> t a b -> c
```


