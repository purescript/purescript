-- @shouldFailWith NoInstanceFound
module Main where

import Effect.Console (log)
import Prim.Row (class Lacks)
import Type.Proxy (Proxy(..))

lacksX
  :: forall r
   . Lacks "x" r
  => Proxy r
  -> Proxy ()
lacksX _ = Proxy

test1 :: Proxy ()
test1 = lacksX (Proxy :: Proxy (x :: Int, y :: Int, z :: String))

main = log "Done"
