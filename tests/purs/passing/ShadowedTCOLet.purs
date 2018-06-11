module Main where

import Prelude
import Partial.Unsafe (unsafePartial)
import Effect
import Effect.Console (log)

f x y z =
  let f 1.0 2.0 3.0 = 1.0
  in f x z y

main :: Effect _
main = do
  log $ show $ unsafePartial f 1.0 3.0 2.0
  log "Done"
