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

lacksSym
  :: forall sym (to :: Row Type)
   . Lacks sym to
  => Proxy sym
  -> Proxy to
lacksSym _ = Proxy

test1 :: Proxy ()
test1 = lacksX (Proxy :: Proxy (y :: Int, z :: String))

test2 :: forall r. Lacks "x" r => Proxy r -> Proxy ()
test2 _ = lacksX (Proxy :: Proxy (y :: Int, z :: String | r))

test3 :: Proxy ()
test3 = test2 (Proxy :: Proxy (a :: String))

test4 :: forall sym. Proxy sym -> Proxy ()
test4 = lacksSym

main = log "Done"
