module Main where

import Prelude
import Effect (whileE)
import Effect.Console (log, logShow)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef

main = do
  ST.run do
    n1 <- STRef.new 1.0
    n2 <- STRef.new 1.0
    ST.while ((>) 1000.0 <$> STRef.read n1) do
      n1' <- STRef.read n1
      n2' <- STRef.read n2
      _ <- STRef.write (n1' + n2') n2
      void $ STRef.write n2' n1
  log "Done"
