module Main where

import Prelude
import Effect.Console (log)

test :: Number -> Boolean
test (-1.0) = false
test _  = true

test2 :: Number -> Number -> Boolean
test2 x y = case x, y of
  -1.0, -1.0 -> false
  _, _ -> true

main = log "Done"
