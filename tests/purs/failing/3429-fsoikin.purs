-- @shouldFailWith CycleInDictDeclaration
-- Example submitted by fsoikin in issue #3488.
module Main where

import Prelude (class Ord, class Semiring, zero, (>))
import Data.Array (filter)

class C a where
  f :: a -> Boolean
  g :: Array a

instance cInst :: (Ord a, Semiring a) => C a where
  f i = i > zero
  g = filter f []
