module Example.Traversable where

import Prelude

import Data.Maybe
import Data.Traversable

main = Console.print $ sequence $ Just [1,2,3]
