module Main where

data Pair a b = Pair a b

instance ordPair :: (Ord a, Ord b) => Ord (Pair a b) where
  compare (Pair a1 b1) (Pair a2 b2) = case compare a1 a2 of
    EQ -> compare b1 b2
    r -> r

instance eqPair :: (Eq a, Eq b) => Eq (Pair a b) where
  (==) (Pair a1 b1) (Pair a2 b2) = a1 == a2 && b1 == b2
  (/=) (Pair a1 b1) (Pair a2 b2) = a1 /= a2 || b1 /= b2

main = Debug.Trace.print $ Pair 1 2 == Pair 1 2
