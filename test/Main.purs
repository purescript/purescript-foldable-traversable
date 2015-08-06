module Test.Main where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console
import Data.Maybe
import Data.Monoid.Additive
import Data.Foldable
import Data.Traversable
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Test.Assert

foreign import arrayFrom1UpTo :: Int -> Array Int

main = do
  log "Test foldableArray instance"
  testFoldableArrayWith 20

  log "Test foldableArray instance is stack safe"
  testFoldableArrayWith 20000

  log "Test foldMapDefaultL"
  testFoldableFoldMapDefaultL 20

  log "Test foldMapDefaultR"
  testFoldableFoldMapDefaultR 20

  log "Test foldMapDefaultT"
  testFoldableFoldMapDefaultT 20

  log "Test foldlDefault"
  testFoldableFoldlDefault 20

  log "Test foldrDefault"
  testFoldableFoldlDefault 20

  log "Test traversableArray instance"
  testTraversableArrayWith 20

  log "Test traversableArray instance is stack safe"
  testTraversableArrayWith 20000

  log "Test traverseDefault"
  testTraverseDefault 20

  log "Test sequenceDefault"
  testSequenceDefault 20

  log "Test mapDefault"
  testMapDefault

  log "Test Bifoldable on `inclusive or`"
  testBifoldableIOrWith id 10 100 42

  log "Test bifoldMapDefaultL"
  testBifoldableIOrWith BFML 10 100 42

  log "Test bifoldMapDefaultR"
  testBifoldableIOrWith BFMR 10 100 42

  log "Test bifoldMapDefaultT"
  testBifoldableIOrWith BFMT 10 100 42

  log "Test bifoldlDefault"
  testBifoldableIOrWith BFLD 10 100 42

  log "Test bifoldrDefault"
  testBifoldableIOrWith BFRD 10 100 42

  log "Test Bitraversable on `inclusive or`"
  testBitraversableIOrWith id

  log "Test bitraverseDefault"
  testBitraversableIOrWith BTD

  log "Test bisequenceDefault"
  testBitraversableIOrWith BSD

  log "Test bimapDefault"
  testBimapDefault

  log "All done!"


testFoldableFWith :: forall f e. (Foldable f, Eq (f Int)) =>
                     (Int -> f Int) -> Int -> Eff (assert :: ASSERT | e) Unit
testFoldableFWith f n = do
  let dat = f n
  let expectedSum = (n / 2) * (n + 1)

  assert $ foldr (+) 0 dat == expectedSum
  assert $ foldl (+) 0 dat == expectedSum
  assert $ foldMap Additive dat == Additive expectedSum

testFoldableArrayWith = testFoldableFWith arrayFrom1UpTo


testTraversableFWith :: forall f e. (Traversable f, Eq (f Int)) =>
                        (Int -> f Int) -> Int -> Eff (assert :: ASSERT | e) Unit
testTraversableFWith f n = do
  let dat = f n

  assert $ traverse Just dat == Just dat
  assert $ traverse return dat == [dat]
  assert $ traverse (\x -> if x < 10 then Just x else Nothing) dat == Nothing
  assert $ sequence (map Just dat) == traverse Just dat

testTraversableArrayWith = testTraversableFWith arrayFrom1UpTo


-- structures for testing default `Foldable` implementations

newtype FoldMapDefaultL a = FML (Array a)
newtype FoldMapDefaultR a = FMR (Array a)
newtype FoldMapDefaultT a = FMT (Array a)
newtype FoldlDefault    a = FLD (Array a)
newtype FoldrDefault    a = FRD (Array a)

instance eqFML :: (Eq a) => Eq (FoldMapDefaultL a) where eq (FML l) (FML r) = l == r
instance eqFMR :: (Eq a) => Eq (FoldMapDefaultR a) where eq (FMR l) (FMR r) = l == r
instance eqFMT :: (Eq a) => Eq (FoldMapDefaultT a) where eq (FMT l) (FMT r) = l == r
instance eqFLD :: (Eq a) => Eq (FoldlDefault a)    where eq (FLD l) (FLD r) = l == r
instance eqFRD :: (Eq a) => Eq (FoldrDefault a)    where eq (FRD l) (FRD r) = l == r

-- implemented `foldl` and `foldr`, but default `foldMap` using `foldl`
instance foldableFML :: Foldable FoldMapDefaultL where
  foldMap f a       = foldMapDefaultL f a
  foldl f u (FML a) = foldl f u a
  foldr f u (FML a) = foldr f u a

-- implemented `foldl` and `foldr`, but default `foldMap`, using `foldr`
instance foldableFMR :: Foldable FoldMapDefaultR where
  foldMap f a       = foldMapDefaultR f a
  foldl f u (FMR a) = foldl f u a
  foldr f u (FMR a) = foldr f u a

-- implemented `foldl` and `foldr`, but default `foldMap`, using `traverse`
instance foldableFMT :: Foldable FoldMapDefaultT where
  foldMap f a       = foldMapDefaultT f a
  foldl f u (FMT a) = foldl f u a
  foldr f u (FMT a) = foldr f u a

-- implemented `foldMap` and `foldr`, but default `foldMap`
instance foldableDFL :: Foldable FoldlDefault where
  foldMap f (FLD a) = foldMap f a
  foldl f u a       = foldlDefault f u a
  foldr f u (FLD a) = foldr f u a

-- implemented `foldMap` and `foldl`, but default `foldr`
instance foldableDFR :: Foldable FoldrDefault where
  foldMap f (FRD a) = foldMap f a
  foldl f u (FRD a) = foldl f u a
  foldr f u a       = foldrDefault f u a

instance functorFMT :: Functor FoldMapDefaultT where
  map f (FMT a) = FMT (map f a)

instance traversableFMT :: Traversable FoldMapDefaultT where
  traverse f (FMT a) = FMT <$> traverse f a
  sequence (FMT a)   = FMT <$> sequence a

testFoldableFoldMapDefaultL = testFoldableFWith (FML <<< arrayFrom1UpTo)
testFoldableFoldMapDefaultR = testFoldableFWith (FMR <<< arrayFrom1UpTo)
testFoldableFoldMapDefaultT = testFoldableFWith (FMT <<< arrayFrom1UpTo)
testFoldableFoldlDefault    = testFoldableFWith (FLD <<< arrayFrom1UpTo)
testFoldableFoldrDefault    = testFoldableFWith (FRD <<< arrayFrom1UpTo)


-- structures for testing default `Traversable` implementations

newtype TraverseDefault a = TD (Array a)
newtype SequenceDefault a = SD (Array a)

instance eqTD :: (Eq a) => Eq (TraverseDefault a) where eq (TD l) (TD r) = l == r
instance eqSD :: (Eq a) => Eq (SequenceDefault a) where eq (SD l) (SD r) = l == r

instance functorTD :: Functor TraverseDefault where map f (TD a) = TD (map f a)
instance functorSD :: Functor SequenceDefault where map f (SD a) = SD (map f a)

instance foldableTD :: Foldable TraverseDefault where
  foldMap f (TD a) = foldMap f a
  foldr f u (TD a) = foldr f u a
  foldl f u (TD a) = foldl f u a

instance foldableSD :: Foldable SequenceDefault where
  foldMap f (SD a) = foldMap f a
  foldr f u (SD a) = foldr f u a
  foldl f u (SD a) = foldl f u a

instance traversableTD :: Traversable TraverseDefault where
  traverse f a    = traverseDefault f a
  sequence (TD a) = map TD (sequence a)

instance traversableSD :: Traversable SequenceDefault where
  traverse f (SD a) = map SD (traverse f a)
  sequence m        = sequenceDefault m

testTraverseDefault = testTraversableFWith (TD <<< arrayFrom1UpTo)
testSequenceDefault = testTraversableFWith (SD <<< arrayFrom1UpTo)


-- structure for testing default `Functor` implementations

newtype MapDefault a = MD (Array a)
instance eqMD :: (Eq a) => Eq (MapDefault a) where eq (MD l) (MD r) = l == r
instance functorMD :: Functor MapDefault     where map f m = mapDefault f m

instance foldableMD :: Foldable MapDefault where
  foldMap f (MD a) = foldMap f a
  foldr f u (MD a) = foldr f u a
  foldl f u (MD a) = foldl f u a

instance traversableMD :: Traversable MapDefault where
  traverse f (MD a) = map MD (traverse f a)
  sequence (MD a)   = map MD (sequence a)

testMapDefault = assert $ map Just (MD [1, 2]) == MD [Just 1, Just 2]


-- structure for testing bifoldable, picked `inclusive or` as it has both products and sums

data IOr l r = Both l r | Fst l | Snd r

instance eqIOr :: (Eq l, Eq r) => Eq (IOr l r) where
  eq (Both lFst lSnd) (Both rFst rSnd) = (lFst == rFst) && (lSnd == rSnd)
  eq (Fst l)          (Fst r)          = l == r
  eq (Snd l)          (Snd r)          = l == r
  eq _                _                = false

instance bifoldableIOr :: Bifoldable IOr where
  bifoldr l r u (Both fst snd) = l fst (r snd u)
  bifoldr l r u (Fst fst)      = l fst u
  bifoldr l r u (Snd snd)      = r snd u

  bifoldl l r u (Both fst snd) = r (l u fst) snd
  bifoldl l r u (Fst fst)      = l u fst
  bifoldl l r u (Snd snd)      = r u snd

  bifoldMap l r (Both fst snd) = l fst <> r snd
  bifoldMap l r (Fst fst)      = l fst
  bifoldMap l r (Snd snd)      = r snd

instance bifunctorIOr :: Bifunctor IOr where
  bimap f g (Both fst snd) = Both (f fst) (g snd)
  bimap f g (Fst fst)      = Fst (f fst)
  bimap f g (Snd snd)      = Snd (g snd)

instance bitraversableIOr :: Bitraversable IOr where
  bitraverse f g (Both fst snd) = Both <$> f fst <*> g snd
  bitraverse f g (Fst fst)      = Fst <$> f fst
  bitraverse f g (Snd snd)      = Snd <$> g snd

  bisequence (Both fst snd) = Both <$> fst <*> snd
  bisequence (Fst fst)      = Fst <$> fst
  bisequence (Snd snd)      = Snd <$> snd

testBifoldableIOrWith :: forall t e. (Bifoldable t, Eq (t Int Int)) =>
                         (forall l r. IOr l r -> t l r) ->
                         Int -> Int -> Int ->
                         Eff (assert :: ASSERT | e) Unit
testBifoldableIOrWith lift fst snd u = do
  assert $ bifoldr (+) (*) u (lift $ Both fst snd) == fst + (snd * u)
  assert $ bifoldr (+) (*) u (lift $ Fst fst)      == fst + u
  assert $ bifoldr (+) (*) u (lift $ Snd snd)      == snd * u

  assert $ bifoldl (+) (*) u (lift $ Both fst snd) == (u + fst) * snd
  assert $ bifoldl (+) (*) u (lift $ Fst fst)      == u + fst
  assert $ bifoldl (+) (*) u (lift $ Snd snd)      == u * snd

  assert $ bifoldMap Additive Additive (lift $ Both fst snd) == Additive (fst + snd)
  assert $ bifoldMap Additive Additive (lift $ Fst fst)      == Additive fst
  assert $ bifoldMap Additive Additive (lift $ Snd snd)      == Additive snd

testBitraversableIOrWith :: forall t e. (Bitraversable t, Eq (t Boolean Boolean)) =>
                        (forall l r. IOr l r -> t l r) ->
                        Eff (assert :: ASSERT | e) Unit
testBitraversableIOrWith lift = do
  let just a = Just (lift a)
  assert $ bisequence (lift (Both (Just true) (Just false))) == just (Both true false)
  assert $ bisequence (lift (Fst (Just true)))               == just (Fst true  :: IOr Boolean Boolean)
  assert $ bisequence (lift (Snd (Just false)))              == just (Snd false :: IOr Boolean Boolean)
  assert $ bitraverse Just Just (lift (Both true false))     == just (Both true false)
  assert $ bitraverse Just Just (lift (Fst true))            == just (Fst true  :: IOr Boolean Boolean)
  assert $ bitraverse Just Just (lift (Snd false))           == just (Snd false :: IOr Boolean Boolean)


-- structures for testing default `Bifoldable` implementations

newtype BifoldMapDefaultL l r = BFML (IOr l r)
newtype BifoldMapDefaultR l r = BFMR (IOr l r)
newtype BifoldMapDefaultT l r = BFMT (IOr l r)
newtype BifoldlDefault    l r = BFLD (IOr l r)
newtype BifoldrDefault    l r = BFRD (IOr l r)

instance eqBFML :: (Eq l, Eq r) => Eq (BifoldMapDefaultL l r) where eq (BFML l) (BFML r) = l == r
instance eqBFMR :: (Eq l, Eq r) => Eq (BifoldMapDefaultR l r) where eq (BFMR l) (BFMR r) = l == r
instance eqBFMT :: (Eq l, Eq r) => Eq (BifoldMapDefaultT l r) where eq (BFMT l) (BFMT r) = l == r
instance eqBFLD :: (Eq l, Eq r) => Eq (BifoldlDefault l r)    where eq (BFLD l) (BFLD r) = l == r
instance eqBFRD :: (Eq l, Eq r) => Eq (BifoldrDefault l r)    where eq (BFRD l) (BFRD r) = l == r

instance bifoldableBFML :: Bifoldable BifoldMapDefaultL where
  bifoldMap f g m        = bifoldMapDefaultL f g m
  bifoldr f g u (BFML m) = bifoldr f g u m
  bifoldl f g u (BFML m) = bifoldl f g u m

instance bifoldableBFMR :: Bifoldable BifoldMapDefaultR where
  bifoldMap f g m        = bifoldMapDefaultR f g m
  bifoldr f g u (BFMR m) = bifoldr f g u m
  bifoldl f g u (BFMR m) = bifoldl f g u m

instance bifoldableBFMT :: Bifoldable BifoldMapDefaultT where
  bifoldMap f g m        = bifoldMapDefaultT f g m
  bifoldr f g u (BFMT m) = bifoldr f g u m
  bifoldl f g u (BFMT m) = bifoldl f g u m

instance bifoldableBFLD :: Bifoldable BifoldlDefault where
  bifoldMap f g (BFLD m) = bifoldMap f g m
  bifoldr f g u (BFLD m) = bifoldr f g u m
  bifoldl f g u m        = bifoldlDefault f g u m

instance bifoldableBFRD :: Bifoldable BifoldrDefault where
  bifoldMap f g (BFRD m) = bifoldMap f g m
  bifoldr f g u m        = bifoldrDefault f g u m
  bifoldl f g u (BFRD m) = bifoldl f g u m

instance bifunctorBFMT :: Bifunctor BifoldMapDefaultT where
  bimap f g (BFMT a) = BFMT (bimap f g a)

instance bitraversableBFMT :: Bitraversable BifoldMapDefaultT where
  bitraverse f g (BFMT a) = BFMT <$> bitraverse f g a
  bisequence (BFMT a)     = BFMT <$> bisequence a


-- structures for testing default `Bitraversable` implementations

newtype BitraverseDefault l r = BTD (IOr l r)
newtype BisequenceDefault l r = BSD (IOr l r)

instance eqBTD :: (Eq l, Eq r) => Eq (BitraverseDefault l r) where eq (BTD l) (BTD r) = l == r
instance eqBSD :: (Eq l, Eq r) => Eq (BisequenceDefault l r) where eq (BSD l) (BSD r) = l == r

instance bifunctorBTD :: Bifunctor BitraverseDefault where bimap f g (BTD m) = BTD (bimap f g m)
instance bifunctorBSD :: Bifunctor BisequenceDefault where bimap f g (BSD m) = BSD (bimap f g m)

instance bifoldableBTD :: Bifoldable BitraverseDefault where
  bifoldMap f g (BTD m) = bifoldMap f g m
  bifoldr f g u (BTD m) = bifoldr f g u m
  bifoldl f g u (BTD m) = bifoldl f g u m

instance bifoldableBSD :: Bifoldable BisequenceDefault where
  bifoldMap f g (BSD m) = bifoldMap f g m
  bifoldr f g u (BSD m) = bifoldr f g u m
  bifoldl f g u (BSD m) = bifoldl f g u m

instance bitraversableBTD :: Bitraversable BitraverseDefault where
  bitraverse f g m   = bitraverseDefault f g m
  bisequence (BTD m) = map BTD (bisequence m)

instance bitraversableBSD :: Bitraversable BisequenceDefault where
  bitraverse f g (BSD m) = map BSD (bitraverse f g m)
  bisequence m           = bisequenceDefault m


-- structure for testing default `Bifunctor` implementation

newtype BimapDefault l r = BMD (IOr l r)

instance eqBMD :: (Eq l, Eq r) => Eq (BimapDefault l r) where
  eq (BMD l) (BMD r) = l == r

instance bifunctorMD :: Bifunctor BimapDefault where
  bimap f g m = bimapDefault f g m

instance bifoldableMD :: Bifoldable BimapDefault where
  bifoldMap f g (BMD a) = bifoldMap f g a
  bifoldr f g u (BMD a) = bifoldr f g u a
  bifoldl f g u (BMD a) = bifoldl f g u a

instance bitraversableBMD :: Bitraversable BimapDefault where
  bitraverse f g (BMD a) = map BMD (bitraverse f g a)
  bisequence (BMD a)     = map BMD (bisequence a)

testBimapDefault = assert $ bimap Just Just (BMD $ Both 1 2) == BMD (Both (Just 1) (Just 2))
