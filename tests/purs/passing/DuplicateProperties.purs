module Main where

import Prelude
import Effect.Console (log)
import Type.Proxy (Proxy(..))

subtractX :: forall r a. Proxy (x :: a | r) -> Proxy r
subtractX Proxy = Proxy

extractX :: forall r a. Proxy (x :: a | r) -> Proxy a
extractX Proxy = Proxy

hasX :: forall r a b. Proxy (x :: a, y :: b | r)
hasX = Proxy

test1 = subtractX (subtractX hasX)

test2
  :: forall r a b
   . Proxy (x :: a, x :: b, x :: Int | r)
  -> Proxy Int
test2 x = extractX (subtractX (subtractX x))

main = log "Done"
