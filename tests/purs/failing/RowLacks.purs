-- @shouldFailWith NoInstanceFound
module Main where

import Effect.Console (log)
import Prim.Row (class Lacks)
import Type.Row (RProxy(..))

lacksX
  :: forall r
   . Lacks "x" r
  => RProxy r
  -> RProxy ()
lacksX _ = RProxy

test1 :: RProxy ()
test1 = lacksX (RProxy :: RProxy (x :: Int, y :: Int, z :: String))

main = log "Done"
