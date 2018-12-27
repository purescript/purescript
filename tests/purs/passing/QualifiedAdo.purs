module Main where

import Prelude
import Effect.Console (log)
import IxApplicative as Ix

testIApplicative :: forall f a. Ix.IxApplicative f => f a a String
testIApplicative = Ix.ado
  a <- Ix.pure "test"
  b <- Ix.pure "test"
  in (a <> b)

testApplicative :: forall f. Applicative f => f String
testApplicative = ado
  a <- pure "test"
  b <- pure "test"
  in (a <> b)

main = log "Done"
