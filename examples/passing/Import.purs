module M1 where

  id :: forall a. a -> a
  id = \x -> x

  foo = id

module M2 where

  import M1

  main = \() -> foo 42
    
module Main where

main = Trace.trace "Done"
