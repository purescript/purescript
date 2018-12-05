module Main where

import Prelude
import Effect.Console (log)
import IxApplicative as Ix

testIndexed :: forall f a. Ix.IxApplicative f => f a a String
testIndexed = Ix.ado
  a <- Ix.pure "test"
  b <- Ix.pure "test"
  in (a <> b)

testMonad :: forall f. Applicative f => f String
testMonad = ado
  a <- pure "test"
  b <- pure "test"
  in (a <> b)

main = log "Done"
