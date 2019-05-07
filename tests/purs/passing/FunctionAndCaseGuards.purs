-- See #3443
module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

-- this is just a really convoluted `const true`
test :: Int -> Boolean
test a
  | false =
      case false of
        true | a > 0 -> true
        _ -> true
  | otherwise = true

main :: Effect Unit
main = do
  if test 0
    then log "Done"
    else pure unit
