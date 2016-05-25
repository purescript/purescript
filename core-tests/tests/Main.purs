module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Test.GenericDeriving as GenericDeriving

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = GenericDeriving.main
