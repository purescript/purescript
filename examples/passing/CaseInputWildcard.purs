module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)

data Foo = X | Y

what ∷ Foo → Int → Boolean → Foo
what x = case _, x, _ of
  0, X, true → X
  0, Y, true → X
  _, _, _ → Y

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let tmp = what Y 0 true
  log "Done"
