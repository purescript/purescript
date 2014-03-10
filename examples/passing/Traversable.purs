module Main where

  import Prelude
  import Data.Maybe
  import Data.Traversable
  import Data.Tuple

  main = Debug.Trace.print (sequence (Tuple 3 (Just 4)))
