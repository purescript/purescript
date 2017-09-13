module Main where

import Prelude
import Type.Data.Symbol (class AppendSymbol)
import Control.Monad.Eff.Console (log)

class Balanced (sym :: Symbol)

instance balanced1 :: Balanced ""
else
instance balanced2
  :: ( AppendSymbol "(" sym1 sym
     , AppendSymbol sym2 ")" sym1
     , Balanced sym2
     ) => Balanced sym

balanced :: forall sym. Balanced sym => @sym -> String
balanced _ = "ok"

b0 :: String
b0 = balanced @""

b1 :: String
b1 = balanced @"()"

b2 :: String
b2 = balanced @"(())"

b3 :: String
b3 = balanced @"((()))"

main = do
  log b0
  log b1
  log b2
  log b3
  log "Done"
