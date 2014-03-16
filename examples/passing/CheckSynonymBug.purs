module Main where

  import Prelude

  type Foo a = [a]  

  foo _ = Data.Array.length ([] :: Foo Number)

  main = Debug.Trace.trace "Done"
