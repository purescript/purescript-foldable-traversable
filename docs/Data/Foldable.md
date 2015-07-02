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

##### Instances
``` purescript
instance foldableArray :: Foldable Array
instance foldableMaybe :: Foldable Maybe
instance foldableFirst :: Foldable First
instance foldableLast :: Foldable Last
instance foldableAdditive :: Foldable Additive
instance foldableDual :: Foldable Dual
instance foldableDisj :: Foldable Disj
instance foldableConj :: Foldable Conj
instance foldableMultiplicative :: Foldable Multiplicative
```

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

#### `mconcat`

``` purescript
mconcat :: forall f m. (Foldable f, Monoid m) => f m -> m
```

Fold a data structure, accumulating values in some `Monoid`.

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

Test whether all `Boolean` values in a data structure are `true`.

#### `or`

``` purescript
or :: forall a f. (Foldable f, BooleanAlgebra a) => f a -> a
```

Test whether any `Boolean` value in a data structure is `true`.

#### `any`

``` purescript
any :: forall a b f. (Foldable f, BooleanAlgebra b) => (a -> b) -> f a -> b
```

Test whether a predicate holds for any element in a data structure.

#### `all`

``` purescript
all :: forall a b f. (Foldable f, BooleanAlgebra b) => (a -> b) -> f a -> b
```

Test whether a predicate holds for all elements in a data structure.

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


