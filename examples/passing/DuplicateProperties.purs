module Main where

import Prelude
import Control.Monad.Eff.Console (log)

data RProxy (r :: # Type) = RProxy

data Proxy (a :: Type) = Proxy

subtractX :: forall r a. RProxy (x :: a | r) -> RProxy r
subtractX RProxy = RProxy

extractX :: forall r a. RProxy (x :: a | r) -> Proxy a
extractX RProxy = Proxy

hasX :: forall r a b. RProxy (x :: a, y :: b | r)
hasX = RProxy

test1 = subtractX (subtractX hasX)

test2
  :: forall r a b
   . RProxy (x :: a, x :: b, x :: Int | r)
  -> Proxy Int
test2 x = extractX (subtractX (subtractX x))

main = log "Done"
