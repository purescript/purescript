module Main where

import Prelude
import Test.Assert

main = do
  assert $ (/ 2.0) 4.0 == 2.0
  assert $ (2.0 /) 4.0 == 0.5
  assert $ (`const` 1.0) 2.0 == 2.0
  assert $ (1.0 `const`) 2.0 == 1.0
  let foo = { x: 2.0 }
  assert $ (/ foo.x) 4.0 == 2.0
  assert $ (foo.x /) 4.0 == 0.5
  let (//) x y = x.x / y.x
  assert $ (// foo { x = 4.0 }) { x: 4.0 } == 1.0
  assert $ (foo { x = 4.0 } //) { x: 4.0 } == 1.0
  Control.Monad.Eff.Console.log "Done!"
