-- @shouldFailWith NoInstanceFound
module Main where

import Safe.Coerce (coerce)

data Rec1 a = Rec1 { f :: a }

arr1ToArr1 :: Rec1 Int -> Rec1 String
arr1ToArr1 = coerce
