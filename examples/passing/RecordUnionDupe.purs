module Main where

import Prelude
import Data.Record (merge)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff (Eff)

main :: forall eff. Eff (console::CONSOLE|eff) Unit
main = do
  log (merge {same:1} {same:"Done"}).same
