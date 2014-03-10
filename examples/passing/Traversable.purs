module Main where

  import Prelude
  import Data.Array
  import Data.Maybe
  import Data.Traversable

  main = Debug.Trace.print (sequence (Just [1,2,3]))
