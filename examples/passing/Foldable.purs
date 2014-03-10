module Main where

  import Prelude
  import Data.Foldable

  sum :: [Number] -> Number
  sum = foldr (+) 0

  main = Debug.Trace.print <<< sum $ [1,2,3,4,5]
