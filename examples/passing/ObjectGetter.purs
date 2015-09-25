module Main where

import Prelude

getX = _.x

point = { x: 1.0, y: 0.0 }

main = do
  Control.Monad.Eff.Console.print $ getX point
  Control.Monad.Eff.Console.log $ _." 123 string Prop Name " { " 123 string Prop Name ": "OK" }
  Control.Monad.Eff.Console.log $ (_.x >>> _.y) { x: { y: "Nested" } }
  Control.Monad.Eff.Console.log $ _.value { value: "Done!" }
