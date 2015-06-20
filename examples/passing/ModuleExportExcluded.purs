module A (module Prelude, foo) where
  import Prelude

  foo :: Number -> Number
  foo _ = 0.0

module Main where
  import Debug.Trace
  import A (foo)

  otherwise = false

  main = do
    print "1.0"
