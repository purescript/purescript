module Main where

import Prelude
import Data.Debug.Type as D
import Data.Debug (class Debug, debug)
import Data.Eq (class Eq1)
import Effect.Console (log)
import Test.Assert

data M f a
  = M0
  | M1 Int Boolean Char String Number (Array String) { foo :: Int }
  | M2 (Array (Array String))
  | M3 { bar :: { baz :: Int } }
  | M4 a
  | M5 (f a)
  | M6 (f (f a))

derive instance eqM :: (Eq1 f, Eq a) => Eq (M f a)
derive instance (Debug (f (f a)), Debug (f a), Debug a) => Debug (M f a)

type MA = M Array

m0 = M0 :: MA Int
m0' = D.constructor "M0" []

m1 = M1 0 true 'a' "b" 1.0 [ "hello", "world" ] { foo: 8 } :: MA Int
m1' = D.constructor "M1"
  [ D.int 0
  , D.boolean true
  , D.char 'a'
  , D.string "b"
  , D.number 1.0
  , D.array [ D.string "hello", D.string "world" ]
  , D.record [ { key: "foo", value: D.int 8 } ]
  ]

m2 = M2 [ [ "a", "b", "c" ], [ "d" ] ] :: MA Int
m2' = D.constructor "M2" 
  [ D.array 
      [ D.array $ map D.string [ "a", "b", "c" ]
      , D.array [ D.string "d" ]
      ]
  ]

m3 = M3 { bar: { baz: 9 } }
m3' = D.constructor "M3" 
  [ D.record
      [ { key: "bar" 
        , value: D.record
            [ { key: "baz"
              , value: D.int 9
              }
            ]
        }
      ]
  ]

m4 = M4 1 :: MA Int
m4' = D.constructor "M4" [ D.int 1 ]

m5 = M5 [ 2 ] :: MA Int
m5' = D.constructor "M5" [ D.array [ D.int 2 ] ]

m6 = M6 [ [ 2 ], [ 4 ] ] :: MA Int
m6' = D.constructor "M6" 
  [ D.array
      [ D.array [ D.int 2 ]
      , D.array [ D.int 4 ]
      ]
  ]

data T a = T (forall t. Show t => t -> a)
derive instance Debug (T a)

main = do
  assert $ debug m1 == m1'
  assert $ debug m2 == m2'
  assert $ debug m3 == m3'
  assert $ debug m4 == m4'
  assert $ debug m5 == m5'
  assert $ debug m6 == m6'

  assert $ debug (T \_ -> 42) == D.constructor "T" [ D.opaque_ "function" ]

  log "Done"
