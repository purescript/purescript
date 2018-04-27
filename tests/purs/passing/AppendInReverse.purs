module Main where

import Prelude
import Data.Symbol (SProxy(..))
import Prim.Symbol (class Append)
import Effect.Console (log)

class Balanced (sym :: Symbol)

instance balanced1 :: Balanced ""
else
instance balanced2
  :: ( Append "(" sym1 sym
     , Append sym2 ")" sym1
     , Balanced sym2
     ) => Balanced sym

balanced :: forall sym. Balanced sym => SProxy sym -> String
balanced _ = "ok"

b0 :: String
b0 = balanced (SProxy :: SProxy "")

b1 :: String
b1 = balanced (SProxy :: SProxy "()")

b2 :: String
b2 = balanced (SProxy :: SProxy "(())")

b3 :: String
b3 = balanced (SProxy :: SProxy "((()))")

main = do
  log b0
  log b1
  log b2
  log b3
  log "Done"
