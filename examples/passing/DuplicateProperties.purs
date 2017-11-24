module Main where

import Prelude
import Control.Monad.Eff.Console (log)

subtractX :: forall r a. @(x :: a | r) -> @r
subtractX _ = @r

extractX :: forall r a. @(x :: a | r) -> @a
extractX _ = @a

hasX :: forall r a b. @(x :: a, y :: b | r)
hasX = @(x :: a, y :: b | r)

test1 = subtractX (subtractX hasX)

test2
  :: forall r a b
   . @(x :: a, x :: b, x :: Int | r)
  -> @Int
test2 x = extractX (subtractX (subtractX x))

main = log "Done"
