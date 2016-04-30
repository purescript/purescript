module Main where

import Prelude

shout = Control.Monad.Eff.Console.log <<< (_ <> "!") <<< show

main = shout "Done"
