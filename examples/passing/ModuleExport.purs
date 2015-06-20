module A (module Prelude) where
  import Prelude

module Main where
  import Debug.Trace
  import A

  main = do
    print (show 1.0)
