-- https://github.com/purescript/purescript/issues/812.0

module Main where

import Prelude
import Debug.Trace

newtype NewType a = NewType (Object a)

rec1 :: Object (a :: Number, b :: Number, c:: Number)
rec1 = { a: 0.0, b: 0.0, c: 0.0 }

rec2 :: NewType (a :: Number, b :: Number, c :: Number)
rec2 = NewType (rec1 { a = 1.0 })

main = trace "Done"
