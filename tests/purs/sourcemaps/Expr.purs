module SourceMaps.Expr where

import Prelude
import Effect (Effect)

type MyRec = { f1 :: Int, f2 :: { f2a:: Int, f2b :: String } }

-- Literals
li :: Int
li = 42

la :: Array Int
la = [ 1, 2, 3 ]

lr :: MyRec
lr = { f1: 3, f2: { f2a: 4, f2b: "Hello" } }

-- Constructor
data MyType = MyTypeC1 Int | MyTypeC2
newtype MyType2 = MyType2 Int

c1 :: MyType
c1 = MyTypeC1 1

c2 :: MyType
c2 = MyTypeC2

c3 :: MyType2
c3 = MyType2 2

-- Obj update
ou :: forall r. { a :: String, a' :: Int | r } -> { a :: String, a' :: Int | r }
ou = _ { a = "Hello", a' = 42 }

-- Accessor
ac1 :: Int
ac1 = lr.f1

ac2 :: { f2a :: Int, f2b :: String }
ac2 = lr.f2

-- Abs
a1 :: Int -> String
a1 = show

a2 :: Int -> String
a2 i = show i

-- App
ap :: String
ap = a1 42

-- Let
l1 :: Int
l1 =
  let f a = 1 + a
  in 3 * (f 2)

l2 :: Int
l2 = 3 * (f 2)
  where
  f a = 1 + a

main :: Effect Unit
main = pure unit
