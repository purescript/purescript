module M1 where

  log x = x

module Main where

  import Prelude
  import Control.Monad.Eff
  import M1
  import qualified Control.Monad.Eff.Console as C

  main = C.log (log "Done")
