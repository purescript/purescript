module A (module A, module Prelude) where
  import Prelude

  type Foo = Boolean

module Main where
  import Debug.Trace
  import A

  bar :: Foo
  bar = true

  main = do
    print (show bar)
