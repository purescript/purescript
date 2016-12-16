module Main where

import Prelude
import Control.Monad.Eff.Console (log)

data OProxy (o :: Ordering) = OProxy

proxyLT :: OProxy LT
proxyLT = OProxy

proxyEQ :: OProxy EQ
proxyEQ = OProxy

proxyGT :: OProxy GT
proxyGT = OProxy

main = log "Done"
