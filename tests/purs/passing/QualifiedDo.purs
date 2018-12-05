module Main where

import Prelude
import Effect.Console (log)
import IxMonad as Ix

testIndexed :: forall m a. Ix.IxMonad m => m a a String
testIndexed = Ix.do
  a <- Ix.pure "test"
  b <- Ix.pure "test"
  Ix.pure (a <> b)

testMonad :: forall m. Monad m => m String
testMonad = do
  a <- pure "test"
  b <- pure "test"
  pure (a <> b)

main = log "Done"
