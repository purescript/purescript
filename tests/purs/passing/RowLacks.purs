module Main where

import Effect.Console (log)
import Prim.Row (class Lacks)
import Type.Row (RProxy(..))

data SProxy (a :: Symbol) = SProxy

lacksX
  :: forall r
   . Lacks "x" r
  => RProxy r
  -> RProxy ()
lacksX _ = RProxy

lacksSym
  :: forall sym (to :: Row Type)
   . Lacks sym to
  => SProxy sym
  -> RProxy to
lacksSym _ = RProxy

test1 :: RProxy ()
test1 = lacksX (RProxy :: RProxy (y :: Int, z :: String))

test2 :: forall r. Lacks "x" r => RProxy r -> RProxy ()
test2 _ = lacksX (RProxy :: RProxy (y :: Int, z :: String | r))

test3 :: RProxy ()
test3 = test2 (RProxy :: RProxy (a :: String))

test4 :: forall sym. SProxy sym -> RProxy ()
test4 = lacksSym

main = log "Done"
