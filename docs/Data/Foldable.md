## Module Data.Foldable

#### `Foldable`

``` purescript
class Foldable f where
  foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldMap :: forall a m. (Monoid m) => (a -> m) -> f a -> m
```

`Foldable` represents data structures which can be _folded_.

- `foldr` folds a structure from the right
- `foldl` folds a structure from the left
- `foldMap` folds a structure by accumulating values in a `Monoid`

Default implementations are provided by the following functions:

- `foldrDefault`
- `foldlDefault`
- `foldMapDefaultR`
- `foldMapDefaultL`

Note: some combinations of the default implementations are unsafe to
use together - causing a non-terminating mutually recursive cycle.
These combinations are documented per function.

##### Instances
``` purescript
Foldable Array
Foldable Maybe
Foldable First
Foldable Last
Foldable Additive
Foldable Dual
Foldable Disj
Foldable Conj
Foldable Multiplicative
```

#### `foldrDefault`

``` purescript
foldrDefault :: forall f a b. (Foldable f) => (a -> b -> b) -> b -> f a -> b
```

A default implementation of `foldr` using `foldMap`.

Note: when defining a `Foldable` instance, this function is unsafe to use
in combination with `foldMapDefaultR`.

#### `foldlDefault`

``` purescript
foldlDefault :: forall f a b. (Foldable f) => (b -> a -> b) -> b -> f a -> b
```

A default implementation of `foldl` using `foldMap`.

Note: when defining a `Foldable` instance, this function is unsafe to use
in combination with `foldMapDefaultL`.

#### `foldMapDefaultR`

``` purescript
foldMapDefaultR :: forall f a m. (Foldable f, Monoid m) => (a -> m) -> f a -> m
```

A default implementation of `foldMap` using `foldr`.

Note: when defining a `Foldable` instance, this function is unsafe to use
in combination with `foldrDefault`.

#### `foldMapDefaultL`

``` purescript
foldMapDefaultL :: forall f a m. (Foldable f, Monoid m) => (a -> m) -> f a -> m
```

A default implementation of `foldMap` using `foldl`.

Note: when defining a `Foldable` instance, this function is unsafe to use
in combination with `foldlDefault`.

#### `fold`

``` purescript
fold :: forall f m. (Foldable f, Monoid m) => f m -> m
```

Fold a data structure, accumulating values in some `Monoid`.

#### `traverse_`

``` purescript
traverse_ :: forall a b f m. (Applicative m, Foldable f) => (a -> m b) -> f a -> m Unit
```

Traverse a data structure, performing some effects encoded by an
`Applicative` functor at each value, ignoring the final result.

For example:

```purescript
traverse_ print [1, 2, 3]
```

#### `for_`

``` purescript
for_ :: forall a b f m. (Applicative m, Foldable f) => f a -> (a -> m b) -> m Unit
```

A version of `traverse_` with its arguments flipped.

This can be useful when running an action written using do notation
for every element in a data structure:

For example:

```purescript
for_ [1, 2, 3] \n -> do
  print n
  trace "squared is"
  print (n * n)
```

#### `sequence_`

``` purescript
sequence_ :: forall a f m. (Applicative m, Foldable f) => f (m a) -> m Unit
```

Perform all of the effects in some data structure in the order
given by the `Foldable` instance, ignoring the final result.

For example:

```purescript
sequence_ [ trace "Hello, ", trace " world!" ]
```

#### `intercalate`

``` purescript
intercalate :: forall f m. (Foldable f, Monoid m) => m -> f m -> m
```

Fold a data structure, accumulating values in some `Monoid`,
combining adjacent elements using the specified separator.

#### `and`

``` purescript
and :: forall a f. (Foldable f, BooleanAlgebra a) => f a -> a
```

The conjunction of all the values in a data structure. When specialized
to `Boolean`, this function will test whether all of the values in a data
structure are `true`.

#### `or`

``` purescript
or :: forall a f. (Foldable f, BooleanAlgebra a) => f a -> a
```

The disjunction of all the values in a data structure. When specialized
to `Boolean`, this function will test whether any of the values in a data
structure is `true`.

#### `all`

``` purescript
all :: forall a b f. (Foldable f, BooleanAlgebra b) => (a -> b) -> f a -> b
```

`all f` is the same as `and <<< map f`; map a function over the structure,
and then get the conjunction of the results.

#### `any`

``` purescript
any :: forall a b f. (Foldable f, BooleanAlgebra b) => (a -> b) -> f a -> b
```

`any f` is the same as `or <<< map f`; map a function over the structure,
and then get the disjunction of the results.

#### `sum`

``` purescript
sum :: forall a f. (Foldable f, Semiring a) => f a -> a
```

Find the sum of the numeric values in a data structure.

#### `product`

``` purescript
product :: forall a f. (Foldable f, Semiring a) => f a -> a
```

Find the product of the numeric values in a data structure.

#### `elem`

``` purescript
elem :: forall a f. (Foldable f, Eq a) => a -> f a -> Boolean
```

Test whether a value is an element of a data structure.

#### `notElem`

``` purescript
notElem :: forall a f. (Foldable f, Eq a) => a -> f a -> Boolean
```

Test whether a value is not an element of a data structure.

#### `find`

``` purescript
find :: forall a f. (Foldable f) => (a -> Boolean) -> f a -> Maybe a
```

Try to find an element in a data structure which satisfies a predicate.

#### `maximum`

``` purescript
maximum :: forall a f. (Ord a, Foldable f) => f a -> Maybe a
```

Find the largest element of a structure, according to its `Ord` instance.

#### `maximumBy`

``` purescript
maximumBy :: forall a f. (Foldable f) => (a -> a -> Ordering) -> f a -> Maybe a
```

Find the largest element of a structure, according to a given comparison
function. The comparison function should represent a total ordering (see
the `Ord` type class laws); if it does not, the behaviour is undefined.

#### `minimum`

``` purescript
minimum :: forall a f. (Ord a, Foldable f) => f a -> Maybe a
```

Find the smallest element of a structure, according to its `Ord` instance.

#### `minimumBy`

``` purescript
minimumBy :: forall a f. (Foldable f) => (a -> a -> Ordering) -> f a -> Maybe a
```

Find the smallest element of a structure, according to a given comparison
function. The comparison function should represent a total ordering (see
the `Ord` type class laws); if it does not, the behaviour is undefined.


