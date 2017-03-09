module Main where

import Prelude
import Data.Record (merge)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff (Eff)
import Unsafe.Coerce (unsafeCoerce)

main :: Eff (console::CONSOLE) Unit
main = do
  let result = (merge {left:1} {left:"Done"}).left
  log $ unsafeCoerce result
