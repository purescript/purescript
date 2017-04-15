module Main where

import Prelude
import Control.Monad.Eff.Console (log)

subtractX :: forall r a. proxy (x :: a | r) -> proxy r
subtractX _ = @r

extractX :: forall r a. proxy (x :: a | r) -> proxy a
extractX _ = @a

hasX :: forall r a b. proxy (x :: a, y :: b | r)
hasX = @(x :: a, y :: b | r)

test1 = subtractX (subtractX hasX)

test2
  :: forall r a b
   . proxy (x :: a, x :: b, x :: Int | r)
  -> proxy Int
test2 x = extractX (subtractX (subtractX x))

main = log "Done"
