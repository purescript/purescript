module Main where

import Prelude
import Effect.Console (log)

class Con

data Identity a = Identity a

test :: Con => Identity (Con => Int) -> Int
test (Identity a) = a

main = do
  log "Done"
