module M1 where

  import Prelude ()

  id :: forall a. a -> a
  id = \x -> x

  foo = id

module M2 where

  import Prelude ()
  import M1

  main = \_ -> foo 42

module Main where

main = Control.Monad.Eff.Console.log "Done"
