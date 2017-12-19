module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import ClassDefinitions

data A

data B

instance inst2 :: C2 A

instance inst4 :: (C2 a) => C3 a B

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Done"
