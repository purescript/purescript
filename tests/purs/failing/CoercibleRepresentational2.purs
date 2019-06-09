-- @shouldFailWith NoInstanceFound
module Main where

import Safe.Coerce (coerce)

data Arr1 a = Arr1 (Array a)

arr1ToArr1 :: Arr1 Int -> Arr1 String
arr1ToArr1 = coerce
