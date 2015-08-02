module Main where

import Prelude
import Test.Assert

test :: forall a b. a -> b -> a
test = \const _ -> const

main = do
  let value = test "Done" {}
  assert' "Not done" $ value == "Done"
  Control.Monad.Eff.Console.log "Done"
