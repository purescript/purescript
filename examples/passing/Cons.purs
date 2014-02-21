module Cons where

  import Prelude
  import Data.Array

  test1 = \xs -> 1 : xs

  test2 = 1 : 2 : 3 : []
    
module Main where

main = Debug.Trace.trace "Done"
