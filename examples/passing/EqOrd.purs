module Main where

import Prelude

data Pair a b = Pair a b

instance ordPair :: (Ord a, Ord b) => Ord (Pair a b) where
  compare (Pair a1 b1) (Pair a2 b2) = case compare a1 a2 of
    EQ -> compare b1 b2
    r -> r

instance eqPair :: (Eq a, Eq b) => Eq (Pair a b) where
  eq (Pair a1 b1) (Pair a2 b2) = a1 == a2 && b1 == b2

main = Control.Monad.Eff.Console.print $ Pair 1.0 2.0 == Pair 1.0 2.0
