module Main where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Console (log)
import Test.Assert (assert)

data Color = Red | Green | Blue
  derive (Eq, Ord)
  derive (Generic)

data Direction = North | South | East | West
  derive (Eq)
  derive (Ord)

data Box a = Box a
  derive (Functor)

showColor :: Color -> String
showColor = genericShow

main = do
  assert $ Red == Red
  assert $ Red < Green
  assert $ North == North
  assert $ North < South
  let Box result = map (_ + 1) (Box 41)
  assert $ result == 42
  assert $ showColor Red == "Red"
  assert $ showColor Blue == "Blue"
  log "Done"
