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
test1 = lacksX (RProxy :: RProxy (y :: Int, z :: String))

test2 :: forall r. Lacks "x" r => RProxy r -> RProxy ()
test2 _ = lacksX (RProxy :: RProxy (y :: Int, z :: String | r))

test3 :: RProxy ()
test3 = test2 (RProxy :: RProxy (a :: String))

main = log "Done"
