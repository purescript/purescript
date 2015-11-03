module Main where

import Prelude
import Control.Monad.Eff.Console

greet { greeting, name } = log $ greeting <> ", " <> name <> "."

main = greet { greeting: "Hello", name: "World" } 
