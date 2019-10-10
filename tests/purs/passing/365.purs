-- Cf. 3429/*.purs and failing/365.purs
module Main where

import Prelude
import Effect.Console (log)

class C a where
  f :: a -> a
  g :: a -> a

instance cS :: C String where
  f s = s
  g s = f s

main = log "Done"
