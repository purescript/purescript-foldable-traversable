
module Test.Main where

import Prelude

import Control.Monad.Eff.Console
import Data.Foldable
import Data.Maybe
import Data.Monoid.Additive
import Data.Traversable
import Test.Assert

foreign import arrayFrom1UpTo :: Int -> Array Int

main = do
  log "Test foldableArray instance"
  testFoldableWith 20

  log "Test traversableArray instance"
  testTraversableWith 20

  log "Test foldableArray instance is stack safe"
  testFoldableWith 20000

  log "Test traversableArray instance is stack safe"
  testTraversableWith 20000

  log "All done!"

testFoldableWith n = do
  let arr = arrayFrom1UpTo n
  let expectedSum = (n / 2) * (n + 1)

  assert $ foldr (+) 0 arr == expectedSum
  assert $ foldl (+) 0 arr == expectedSum
  assert $ foldMap Additive arr == Additive expectedSum

testTraversableWith n = do
  let arr = arrayFrom1UpTo n

  assert $ traverse Just arr == Just arr
  assert $ traverse return arr == [arr]
  assert $ traverse (\x -> if x < 10 then Just x else Nothing) arr == Nothing
