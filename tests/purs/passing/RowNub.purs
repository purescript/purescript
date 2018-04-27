module Main where

import Effect.Console (log)
import Prim.Row (class Nub, class Union)
import Type.Row (RProxy(..))

nubUnion
  :: forall r1 r2 r3 r4
   . Union r1 r2 r3
  => Nub r3 r4
  => RProxy r1
  -> RProxy r2
  -> RProxy r4
nubUnion _ _ = RProxy

type InL = (x :: Int, y :: String)
type InR = (x :: String, y :: Int, z :: Boolean)
type Out = (x :: Int, y :: String, z :: Boolean)

test :: RProxy Out
test = nubUnion (RProxy :: RProxy InL) (RProxy :: RProxy InR)

main = log "Done"
