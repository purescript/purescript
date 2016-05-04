module Main where

import Prelude
import A (foo)
import B (Foo(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let tmp = foo X
  log "Done"
