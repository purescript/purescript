module M1 where

  id :: forall a. a -> a
  id = \x -> x

  foo = id

module M2 where

  import M1

  main = \_ -> foo 42
    
module Main where

main = Debug.Trace.trace "Done"
