module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Bifoldable (class Bifoldable, bifoldl, bifoldr, bifoldMap, bifoldrDefault, bifoldlDefault, bifoldMapDefaultR, bifoldMapDefaultL)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Bitraversable (class Bitraversable, bisequenceDefault, bitraverse, bisequence, bitraverseDefault)
import Data.Foldable (class Foldable, find, findMap, fold, foldMap, foldMapDefaultL, foldMapDefaultR, foldl, foldlDefault, foldr, foldrDefault, length, maximum, maximumBy, minimum, minimumBy, null, surroundMap)
import Data.Function (on)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Additive (Additive(..))
import Data.Traversable (class Traversable, sequenceDefault, traverse, sequence, traverseDefault)
import Math (abs)
import Test.Assert (ASSERT, assert)

foreign import arrayFrom1UpTo :: Int -> Array Int

main :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
main = do
  log "Test foldableArray instance"
  assert $ foldMap Additive (arrayFrom1UpTo 20) == Additive (20 * 21 / 2)
  testFoldableFWith (arrayFrom1UpTo 20) (\x -> [x])
  testFoldableFWith (arrayFrom1UpTo 20) (\x -> Additive x)

  log "Test foldableArray instance is stack safe"
  -- can't use testFoldableFWith because foldMapDefaultL and -R are not stack
  -- safe
  _ <- pure $ foldMap Additive (arrayFrom1UpTo 20000)
  _ <- pure $ fold (Additive <$> arrayFrom1UpTo 20000)
  _ <- pure $ foldMapDefaultL Additive (arrayFrom1UpTo 20000)
  _ <- pure $ foldMapDefaultR Additive (arrayFrom1UpTo 20000)

  log "Test traversableArray instance"
  testTraversableArrayWith 20

  log "Test traversableArray instance is stack safe"
  testTraversableArrayWith 20000

  log "Test traverseDefault"
  testTraverseDefault 20

  log "Test sequenceDefault"
  testSequenceDefault 20

  log "Test Bifoldable on `inclusive or`"
  testBifoldableIOrWith id 10 100 42

  log "Test bifoldMapDefaultL"
  testBifoldableIOrWith BFML 10 100 42

  log "Test bifoldMapDefaultR"
  testBifoldableIOrWith BFMR 10 100 42

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

  log "Test find"
  assert $ find (_ == 10) [1, 5, 10] == Just 10
  assert $ find (\x -> x `mod` 2 == 0) [1, 4, 10] == Just 4

  log "Test findMap" *> do
    let pred x = if x > 5 then Just (x * 100) else Nothing
    assert $ findMap pred [1, 5, 10, 20] == Just 1000

  log "Test maximum"
  assert $ maximum (arrayFrom1UpTo 10) == Just 10

  log "Test maximumBy"
  assert $
    maximumBy (compare `on` abs)
              (map (negate <<< toNumber) (arrayFrom1UpTo 10))
      == Just (-10.0)

  log "Test minimum"
  assert $ minimum (arrayFrom1UpTo 10) == Just 1

  log "Test minimumBy"
  assert $
    minimumBy (compare `on` abs)
              (map (negate <<< toNumber) (arrayFrom1UpTo 10))
      == Just (-1.0)

  log "Test null"
  assert $ null Nothing == true
  assert $ null (Just 1) == false
  assert $ null [] == true
  assert $ null [0] == false
  assert $ null [0,1] == false

  log "Test length"
  assert $ length Nothing == 0
  assert $ length (Just 1) == 1
  assert $ length [] == 0
  assert $ length [1] == 1
  assert $ length [1, 2] == 2

  log "Test surroundMap"
  assert $ "*" == surroundMap "*" show ([] :: Array Int)
  assert $ "*1*" == surroundMap "*" show [1]
  assert $ "*1*2*" == surroundMap "*" show [1, 2]
  assert $ "*1*2*3*" == surroundMap "*" show [1, 2, 3]

  log "All done!"


testFoldableFWith
  :: forall f m a e
   . Foldable f
  => Functor f
  => Monoid m
  => Eq m
  => f a
  -> (a -> m)
  -> Eff (assert :: ASSERT | e) Unit
testFoldableFWith n f = do
  let m = foldMap f n
  assert $ foldMapDefaultR f n == m
  assert $ foldMapDefaultL f n == m
  assert $ foldlDefault (\y x -> y <> f x) mempty n == m
  assert $ foldrDefault (\x y -> f x <> y) mempty n == m
  assert $ fold (f <$> n) == m

testTraversableFWith
  :: forall f e
   . Traversable f
  => Eq (f Int)
  => (Int -> f Int)
  -> Int
  -> Eff (assert :: ASSERT | e) Unit
testTraversableFWith f n = do
  let dat = f n

  assert $ traverse Just dat == Just dat
  assert $ traverse pure dat == [dat]
  assert $ traverse (\x -> if x < 10 then Just x else Nothing) dat == Nothing
  assert $ sequence (map Just dat) == traverse Just dat

testTraversableArrayWith :: forall eff. Int -> Eff (assert :: ASSERT | eff) Unit
testTraversableArrayWith = testTraversableFWith arrayFrom1UpTo

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
  traverse f      = traverseDefault f
  sequence (TD a) = map TD (sequence a)

instance traversableSD :: Traversable SequenceDefault where
  traverse f (SD a) = map SD (traverse f a)
  sequence m        = sequenceDefault m

testTraverseDefault :: forall eff. Int -> Eff (assert :: ASSERT | eff) Unit
testTraverseDefault = testTraversableFWith (TD <<< arrayFrom1UpTo)

testSequenceDefault :: forall eff. Int -> Eff (assert :: ASSERT | eff) Unit
testSequenceDefault = testTraversableFWith (SD <<< arrayFrom1UpTo)


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

testBifoldableIOrWith
  :: forall t e
   . Bifoldable t
  => Eq (t Int Int)
  => (forall l r. IOr l r -> t l r)
  -> Int
  -> Int
  -> Int
  -> Eff (assert :: ASSERT | e) Unit
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

testBitraversableIOrWith
  :: forall t e
   . Bitraversable t
  => Eq (t Boolean Boolean)
  => (forall l r. IOr l r -> t l r)
  -> Eff (assert :: ASSERT | e) Unit
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
newtype BifoldlDefault    l r = BFLD (IOr l r)
newtype BifoldrDefault    l r = BFRD (IOr l r)

instance eqBFML :: (Eq l, Eq r) => Eq (BifoldMapDefaultL l r) where eq (BFML l) (BFML r) = l == r
instance eqBFMR :: (Eq l, Eq r) => Eq (BifoldMapDefaultR l r) where eq (BFMR l) (BFMR r) = l == r
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

instance bifoldableBFLD :: Bifoldable BifoldlDefault where
  bifoldMap f g (BFLD m) = bifoldMap f g m
  bifoldr f g u (BFLD m) = bifoldr f g u m
  bifoldl f g u m        = bifoldlDefault f g u m

instance bifoldableBFRD :: Bifoldable BifoldrDefault where
  bifoldMap f g (BFRD m) = bifoldMap f g m
  bifoldr f g u m        = bifoldrDefault f g u m
  bifoldl f g u (BFRD m) = bifoldl f g u m


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
  bitraverse f g     = bitraverseDefault f g
  bisequence (BTD m) = map BTD (bisequence m)

instance bitraversableBSD :: Bitraversable BisequenceDefault where
  bitraverse f g (BSD m) = map BSD (bitraverse f g m)
  bisequence m           = bisequenceDefault m
