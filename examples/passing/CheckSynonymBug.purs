module Foo where

  import Prelude

  type Foo a = [a]  

  foo _ = Data.Array.length ([] :: Foo Number)

module Main where

main = Debug.Trace.trace "Done"
