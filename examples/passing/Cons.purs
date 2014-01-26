module Cons where

  import Prelude
  import Arrays

  test1 = \xs -> 1 : xs

  test2 = 1 : 2 : 3 : []
    
module Main where

main = Trace.trace "Done"
