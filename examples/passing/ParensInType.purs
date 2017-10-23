module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

class Foo a where
  foo :: forall eff. (String -> a (( console :: CONSOLE | eff)) ((Unit)))

instance fooLogEff :: Foo Eff where
  foo = log

main :: 
    forall eff.
      Eff
        ( console :: CONSOLE
        | eff
        )
        Unit
main = foo "Done"
