module Main where

import Prelude

collatz = \x -> case x of
  y | y `mod` 2.0 == 0.0 -> y / 2.0
  y -> y * 3.0 + 1.0

-- Guards have access to current scope
collatz2 = \x y -> case x of
  z | y > 0.0 -> z / 2.0
  z -> z * 3.0 + 1.0

min :: forall a. (Ord a) => a -> a -> a
min n m | n < m     = n
        | otherwise = m

max :: forall a. (Ord a) => a -> a -> a
max n m = case unit of
  _ | m < n     -> n
    | otherwise -> m

testIndentation :: Number -> Number -> Number
testIndentation x y | x > 0.0
  = x + y
                    | otherwise
  = y - x

main = Control.Monad.Eff.Console.log $ min "Done" "ZZZZ"
