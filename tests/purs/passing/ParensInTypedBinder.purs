module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

foo :: Array Int
foo = do
  xss :: Array (Array Int) <- [[[1,2,3], [4, 5]], [[6]]]
  xs :: Array Int <- xss
  xs

main :: Effect Unit
main = log "Done"
