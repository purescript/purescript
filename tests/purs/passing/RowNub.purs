module Main where

import Effect.Console (log)
import Prim.Row (class Nub, class Union)
import Type.Proxy (Proxy(..))

nubUnion
  :: forall r1 r2 r3 r4
   . Union r1 r2 r3
  => Nub r3 r4
  => Proxy r1
  -> Proxy r2
  -> Proxy r4
nubUnion _ _ = Proxy

type InL = (x :: Int, y :: String)
type InR = (x :: String, y :: Int, z :: Boolean)
type Out = (x :: Int, y :: String, z :: Boolean)

test :: Proxy Out
test = nubUnion (Proxy :: Proxy InL) (Proxy :: Proxy InR)

main = log "Done"
