module Main where

import Prelude
import Control.Monad.Eff.Console

greet { greeting, name } = log $ greeting <> ", " <> name <> "."

main = greet { greeting, name} where
  greeting = "Hello"
  name = "World"
