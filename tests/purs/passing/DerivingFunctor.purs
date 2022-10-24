module Main where

import Prelude
import Data.Eq (class Eq1)
import Effect.Console (log)
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..))
import Test.Assert

type RecordFields f a =
  { a :: a
  , zArrayA :: Array a
  , fa :: f a
  , ignore :: Int
  , recursiveA :: Array (Tuple Int (Array a))
  , arrayIgnore :: Array Int
  , fIgnore :: f Int
  , empty :: {}
  }

data M f a
  = M0 a (Array a)
  | M1 Int
  | M2 (f a)
  | M3 (RecordFields f a)
  | M4 { nested :: RecordFields f a }
  | M5 Int a (Array Int) (Array a) (f a) (f Int) (RecordFields f a) { nested :: RecordFields f a }
  | M6 (Array (Array (Array a)))

derive instance eqM :: (Eq1 f, Eq a) => Eq (M f a)
derive instance functorM :: Functor f => Functor (M f)

type MA = M Array

m0L = M0 0 [1, 2] :: MA Int
m0R = M0 "0" ["1", "2"] :: MA String

m1L = M1 0 :: MA Int
m1R = M1 0 :: MA String

m2L = M2 [0, 1] :: MA Int
m2R = M2 ["0", "1"] :: MA String

m3L = M3 recordValueL :: MA Int
m3R = M3 recordValueR :: MA String

m4L = M4 { nested: recordValueL } :: MA Int
m4R = M4 { nested: recordValueR } :: MA String

m5L = M5 0 1 [2, 3] [3, 4] [5, 6] [7, 8] recordValueL { nested: recordValueL } :: MA Int
m5R = M5 0 "1" [2, 3] ["3", "4"] ["5", "6"] [7, 8] recordValueR { nested: recordValueR } :: MA String

recordValueL :: RecordFields Array Int
recordValueL = { a: 71, zArrayA: [72], fa: [73], ignore: 91, recursiveA: [ Tuple 1 [1], Tuple 2 [2] ], arrayIgnore: [92, 93], fIgnore: [94], empty: {} }

recordValueR :: RecordFields Array String
recordValueR = { a: "71", zArrayA: ["72"], fa: ["73"], ignore: 91, recursiveA: [ Tuple 1 ["1"], Tuple 2 ["2"] ], arrayIgnore: [92, 93], fIgnore: [94], empty: {} }

m6L = M6 [[[1, 2]]] :: MA Int
m6R = M6 [[["1", "2"]]] :: MA String

maTests = do
  assert' "map - M0" $ map show m0L == m0R
  assert' "map - M1" $ map show m1L == m1R
  assert' "map - M2" $ map show m2L == m2R
  assert' "map - M3" $ map show m3L == m3R
  assert' "map - M4" $ map show m4L == m4R
  assert' "map - M5" $ map show m5L == m5R
  assert' "map - M6" $ map show m6L == m6R

data Fun1 a = Fun1 (Int -> Int -> a)
derive instance Functor Fun1

f1Test = do
  assert' "map - Fun1" do
    let
      fn = show
      left a b = a + b
      right a b = fn $ left a b
      Fun1 left' = map fn $ Fun1 left
    left' 1 2 == right 1 2

data Fun2 a = Fun2 (Int -> Int -> Array (Array a))
derive instance Functor Fun2

f2Test = do
  assert' "map - Fun2" do
    let
      fn = show
      left a b = [[a + b]]
      right a b = map (map fn) $ left a b
      Fun2 left' = map fn $ Fun2 left
    left' 1 2 == right 1 2

data Fun3 f a = Fun3 (Unit -> Array (f (Array { nested :: RecordFields f a })))
derive instance Functor f => Functor (Fun3 f)

f3Test = do
  assert' "map - Fun3" do
    let
      left _ = [[[{ nested: recordValueL }]]]
      right _ = [[[{ nested: recordValueR }]]]
      Fun3 left' = map show $ Fun3 left
    left' unit == right unit

data T a = T (forall t. Show t => t -> a)
derive instance functorT :: Functor T

taTests = do
  case map show (T \_ -> 42) of
    T f -> assert' "map show T" $ f "hello" == "42"
    _   -> assert' "map show T" false

funTests = do
  f1Test
  f2Test
  f3Test
  taTests

main = do
  maTests
  funTests

  log "Done"
