module Main where

import Prelude hiding (add)
import Control.Monad.Eff.Console (log)

runNat f = f 0.0 (\n -> n + 1.0)

zero' z _ = z

succ f zero' succ = succ (f zero' succ)

add f g zero' succ = g (f zero' succ) succ

one' = succ zero'
two = succ one'
four = add two two
fourNumber = runNat four

main = do
  log $ show fourNumber
  log "Done"
