module Main where

import Prelude

import Data.Predicate (Predicate)
import Effect.Console (log)

data Test a
  = Test1 (Predicate (Predicate a))
  | Test2 { x :: Predicate { y :: Predicate a } }
derive instance Functor Test

main = log "Done"
