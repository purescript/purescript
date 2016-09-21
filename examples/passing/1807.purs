module Main where

import Prelude
import Control.Monad.Eff.Console (log)

fn = _.b.c.d
a = {b:{c:{d:2}}}

d :: Int
d = fn a + a.b.c.d

main = if fn a + a.b.c.d == 4
  then log "Done"
  else log "Fail"
