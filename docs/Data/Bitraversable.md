## Module Data.Bitraversable

#### `Bitraversable`

``` purescript
class (Bifunctor t, Bifoldable t) <= Bitraversable t where
  bitraverse :: forall f a b c d. (Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
  bisequence :: forall f a b. (Applicative f) => t (f a) (f b) -> f (t a b)
```

`Bitraversable` represents data structures with two type arguments which can be
traversed.

A traversal for such a structure requires two functions, one for each type
argument. Type class instances should choose the appropriate function based
on the type of the element encountered at each point of the traversal.

Default implementations are provided by the following functions:

- `bitraverseDefault`
- `bisequenceDefault`

#### `bitraverseDefault`

``` purescript
bitraverseDefault :: forall t f a b c d. (Bitraversable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
```

A default implementation of `bitraverse` using `bisequence` and `bimap`.

#### `bisequenceDefault`

``` purescript
bisequenceDefault :: forall t f a b. (Bitraversable t, Applicative f) => t (f a) (f b) -> f (t a b)
```

A default implementation of `bisequence` using `bitraverse`.

#### `bifor`

``` purescript
bifor :: forall t f a b c d. (Bitraversable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f (t c d)
```

Traverse a data structure, accumulating effects and results using an `Applicative` functor.


