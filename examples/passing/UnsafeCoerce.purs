module Main where

import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

x :: Number
x = unsafeCoerce 1

y :: Number
y = case unsafeCoerce 1 of
  z -> unsafeCoerce z

main :: Eff (console :: CONSOLE) Unit
main = log "Done"
