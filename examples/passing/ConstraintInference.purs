module Main where

import Prelude

shout = Control.Monad.Eff.Console.log <<< (<> "!") <<< show

main = shout "Done"
