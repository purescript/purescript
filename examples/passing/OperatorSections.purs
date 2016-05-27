module Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Test.Assert

main = do
  assert $ (_ / 2.0) 4.0 == 2.0
  assert $ (2.0 / _) 4.0 == 0.5
  assert $ (_ `const` 1.0) 2.0 == 2.0
  assert $ (1.0 `const` _) 2.0 == 1.0
  let foo = { x: 2.0 }
  assert $ (_ / foo.x) 4.0 == 2.0
  assert $ (foo.x / _) 4.0 == 0.5
  let div x y = x.x / y.x
  assert $ (_ `div` foo { x = 4.0 }) { x: 4.0 } == 1.0
  assert $ (foo { x = 4.0 } `div` _) { x: 4.0 } == 1.0
  log "Done"
