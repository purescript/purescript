module Main where

import Prelude
import Type.Data.Symbol (class AppendSymbol, SProxy(..))
import Control.Monad.Eff.Console (log)

class Balanced (sym :: Symbol)

instance balanced1 :: Balanced ""
instance balanced2
  :: ( AppendSymbol "(" sym1 sym
     , AppendSymbol sym2 ")" sym1
     , Balanced sym2
     ) => Balanced sym

balanced :: forall sym. Balanced sym => SProxy sym -> String
balanced _ = "ok"

main = do
  log (balanced (SProxy :: SProxy ""))
  log (balanced (SProxy :: SProxy "()"))
  log (balanced (SProxy :: SProxy "(())"))
  log (balanced (SProxy :: SProxy "((()))"))
  log "Done"
