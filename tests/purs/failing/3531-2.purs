-- @shouldFailWith NoInstanceFound
module Main where

import Prim.TypeError (class Fail, Text)

class C x where
  thing :: x -> x

data X a b = X

test1 :: forall a. X a Int
test1 = X

instance cx :: C (X x x) where
  thing x = x

else instance cxFail :: Fail (Text "Fell through") => C (X x y) where
  thing x = x

test2 :: Boolean
test2 = do
  let X = thing test1
  true
