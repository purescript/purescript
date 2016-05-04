module Main where

import Prelude
import Control.Monad.Eff.Console (log, logShow)

data L a = C a (L a) | N

test :: Number -> L Number -> Number
test n N = n
test n (C x xs) = test (n + x) xs

loop :: forall a. Number -> a
loop x = loop (x + 1.0)

notATailCall = \x ->
  (\notATailCall -> notATailCall x) (\x -> x)

main = do
  logShow (test 0.0 (1.0 `C` (2.0 `C` (3.0 `C` N))))
  log "Done"
