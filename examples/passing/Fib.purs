module Main where

import Prelude
import Effect.Console (log)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef

fib :: Number
fib = ST.run do
    n1 <- STRef.new 1.0
    n2 <- STRef.new 1.0
    ST.while ((>) 1000.0 <$> STRef.read n1) do
      n1' <- STRef.read n1
      n2' <- STRef.read n2
      _ <- STRef.write (n1' + n2') n2
      STRef.write n2' n1
    STRef.read n2

main = do
  log "Done"
