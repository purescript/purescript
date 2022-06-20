module Main where

import Prelude
import Prim.Symbol (class Append)
import Effect.Console (log)
import Type.Proxy (Proxy(..))

class Balanced (sym :: Symbol)

instance balanced1 :: Balanced ""
else
instance balanced2
  :: ( Append "(" sym1 sym
     , Append sym2 ")" sym1
     , Balanced sym2
     ) => Balanced sym

balanced :: forall sym. Balanced sym => Proxy sym -> String
balanced _ = "ok"

b0 :: String
b0 = balanced (Proxy :: Proxy "")

b1 :: String
b1 = balanced (Proxy :: Proxy "()")

b2 :: String
b2 = balanced (Proxy :: Proxy "(())")

b3 :: String
b3 = balanced (Proxy :: Proxy "((()))")

main = do
  log b0
  log b1
  log b2
  log b3
  log "Done"
