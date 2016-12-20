module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

test x = show (x :: _ :: *)

main = do
  when (show (unit :: Unit :: *) == "unit") (log "Done")
  when (test unit == "unit") (log "Done")
