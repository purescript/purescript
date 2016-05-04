module Main where

import Prelude
import Control.Monad.Eff.Console (log, logShow)

class Su a where
  su :: a -> a

class (Su a) <= Cl a where
  cl :: a -> a -> a

instance suNumber :: Su Number where
  su n = n + 1.0

instance clNumber :: Cl Number where
  cl n m = n + m

test :: forall a. (Cl a) => a -> a
test a = su (cl a a)

main = do
  logShow $ test 10.0
  log "Done"
