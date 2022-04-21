module SourceMaps.Binders where

import Prelude
import Effect (Effect)

f1 :: String
f1 = "literal"

  where
  -- Literals

  intb = case _ of
    42 -> "int"
    _ -> ""
  numb = case _ of
    42.0 -> "number"
    _ -> ""
  strb = case _ of
    "foo" -> "string"
    _ -> ""
  chrb = case _ of
    'f' -> "char"
    _ -> ""
  boolb = case _ of
    true -> "bool true"
    false -> "bool false"
  recb = case _ of
    { a: 42 } -> "record"
    _ -> ""
  arrb = case _ of
    [ 1, 2, _ ] -> "array"
    _ -> ""
  
f2 :: String -> Int -> String
f2 a b = case a, b of

  -- Var 
  a, 42 -> "var " <> a

  _, _ -> ""


f3 :: Int -> Int -> String
f3 a b = case a, b of

  -- Null
  _, 42 -> "null"

  _, _ -> ""


data MyType1 = C1 Int | C2 | C3 | C4
newtype MyType2 = MyType2 Int

f4 :: MyType1 -> MyType2 -> String
f4 a b = case a, b of

  -- Named
  c1@(C1 _), _ -> "named"

  -- Constructor
  C2, _ -> "constructor C2" 
  C3, MyType2 _ -> "constructor MyType2"

  _, _ -> ""


main :: Effect Unit
main = pure unit