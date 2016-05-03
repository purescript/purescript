module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Test.Assert

mkRecord = { foo: _, bar: _, baz: "baz" }

getValue :: forall e. Eff (| e) Boolean
getValue = pure true

main = do
  obj <- { value: _ } <$> getValue
  logShow obj.value
  let x = 1.0
  point <- { x: _, y: x } <$> pure 2.0
  assert $ point.x == 2.0
  assert $ point.y == 1.0
  log (mkRecord 1.0 "Done").bar
