module Main where

import Prelude

data A = A | B

instance eqA1 :: Eq A where
  eq A A = true
  eq B B = true
  eq _ _ = false

instance eqA2 :: Eq A where
  eq _ _ = true

instance ordA :: Ord A where
  compare A B = LT
  compare B A = GT
  compare _ _ = EQ

test :: forall a. (Ord a) => a -> a -> String
test x y = show $ x == y

main = Test.Assert.assert $ test A B == "false"
