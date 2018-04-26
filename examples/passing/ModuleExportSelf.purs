module Main where

import Effect.Console
import A

bar :: Foo
bar = true

main = do
  logShow (show bar)
  log "Done"
