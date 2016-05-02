module Main where

import Prelude
import Control.Monad.Eff.Console (log)

f x y z =
  let f 1.0 2.0 3.0 = 1.0
  in f x z y

main = do
  log $ show $ f 1.0 3.0 2.0
  log "Done"
