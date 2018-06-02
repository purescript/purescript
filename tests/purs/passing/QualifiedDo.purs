module Main where

import Prelude
import Effect.Console (log)
import IxMonad as I

testIndexed :: forall m a. I.IxMonad m => m a a String
testIndexed = I.do
  a <- I.pure "test"
  b <- I.pure "test"
  I.pure b

testMonad :: forall m. Monad m => m String
testMonad = do
  a <- pure "test"
  b <- pure "test"
  pure b

main = log "Done"
