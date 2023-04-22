module Main where

import Prelude

import Data.Functor.Contravariant (class Contravariant)
import Data.Predicate (Predicate)
import Data.Tuple (Tuple)
import Effect.Console (log)

data Test f a
  = Test0
  | Test1 (Predicate a)
  | Test2 (Predicate (Predicate (Predicate a)))
  | Test3 Int (forall a. Array a -> Array a)
  | Test4 Int (f a)
  | Test5 (Array (a -> Int)) (Tuple (Predicate a) Int)
  | Test6 { nested :: Array { x :: f { a :: a } } }
derive instance Contravariant f => Contravariant (Test f)

main = log "Done"
