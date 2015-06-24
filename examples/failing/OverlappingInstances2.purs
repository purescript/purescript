module OverlappingInstances where

import Prelude

data A = A | B

instance eqA1 :: Eq A where
  (==) A A = true
  (==) B B = true
  (==) _ _ = false
  (/=) x y = not (x == y)

instance eqA2 :: Eq A where
  (==) _ _ = true
  (/=) _ _ = false

instance ordA :: Ord A where
  compare A B = LT
  compare B A = GT
  compare _ _ = EQ

test :: forall a. (Ord a) => a -> a -> String
test x y = show $ x == y

main = Debug.Trace.trace $ test A B
