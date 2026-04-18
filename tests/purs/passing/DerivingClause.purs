module Main where

import Prelude

import Data.Bifunctor (class Bifunctor, bimap)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Foldable (class Foldable, foldMap)
import Data.Traversable (class Traversable)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert

data Color = Red | Green | Blue
  derive (Eq, Ord)

newtype Name = Name String
  derive (Eq, Ord)

data List a = Nil | Cons a (List a)
  derive (Functor, Foldable, Traversable)

data Either2 a b = Left2 a | Right2 b
  derive (Bifunctor)

derive instance Eq a => Eq (Either2 a a)

data Direction = North | South | East | West
  derive (Generic)

newtype Wrapper = Wrapper String
  derive (Newtype)

data Pair a = Pair a a
  derive (Functor)

data Box a = Empty | Full a
  derive (Functor)

derive instance Eq a => Eq (Box a)

main :: Effect Unit
main = do
  assert $ Red == Red
  assert $ Red < Green
  assert $ Name "Alice" == Name "Alice"
  assert $ foldMap show (Cons 1 (Cons 2 Nil)) == "12"
  assert $ bimap (_ + 1) (_ * 2) (Left2 3) == Left2 4
  assert $ map (_ + 1) (Full 1) == Full 2
  assert $ case map (_ + 1) (Pair 1 2) of
    Pair 2 3 -> true
    _ -> false
  log "Done"
