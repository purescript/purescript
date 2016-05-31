module Main where

import Prelude
import Control.Monad.Eff.Console (log, logShow)

getX = _.x

point = { x: 1.0, y: 0.0 }

main = do
  logShow $ getX point
  log $ _." 123 string Prop Name " { " 123 string Prop Name ": "OK" }
  log $ (_.x >>> _.y) { x: { y: "Nested" } }
  log $ _.value { value: "Done" }
