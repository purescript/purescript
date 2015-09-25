module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Test.Assert

foo foo = let foo_1 = \_ -> foo
              foo_2 = foo_1 unit + 1.0
          in foo_2

main = do
  assert $ foo 1.0 == 2.0
  log "Done"
