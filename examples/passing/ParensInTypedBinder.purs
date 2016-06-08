module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

foo :: Array Int
foo = do
  xss :: Array (Array Int) <- [[[1,2,3], [4, 5]], [[6]]]
  xs :: Array Int <- xss
  xs

main :: 
    forall eff.
      Eff
        ( console :: CONSOLE
        | eff
        )
        Unit
main = log "Done"
