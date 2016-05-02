module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Test.Assert

getValue :: forall e. Eff (| e) Boolean
getValue = pure true

main = do
  let record = { value: false }
  record' <- record { value = _ } <$> getValue
  assert $ record'.value == true

  let point = { x: 1.0, y: 1.0 }
      x = 10.0
      point' = (point { x = _, y = x }) 100.0

  assert $ point'.x == 100.0
  assert $ point'.y == 10.0

  let record2 = (_ { x = _ }) { x: 0.0 } 10.0
  assert $ record2.x == 10.0

  log "Done"
