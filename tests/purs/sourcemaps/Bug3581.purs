module SourceMaps.Bug3581 where

import Prelude

import Effect (Effect)
import Effect.Console (log)

foo :: Int -> Int -> Int
foo 0 y = y
foo x 0 = x
foo a b = a + b

log' :: String -> Effect Unit
log' = log

main :: Effect Unit
main = do
  let x = 5
  log' $ "x = " <> show x
  log' "Hello, world"
  let y = 10
  let z = foo 5 $ foo 10 15
  log $ "z = " <> show z
  log $ append "z = " $ show z
  log "🍝"