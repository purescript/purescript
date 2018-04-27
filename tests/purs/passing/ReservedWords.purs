-- See https://github.com/purescript/purescript/issues/606
module Main where

import Prelude
import Effect
import Effect.Console (log)

o :: { type :: String }
o = { type: "o" }

p :: { type :: String }
p = o { type = "p" }

f :: forall r. { type :: String | r } -> String
f { type: "p" } = "Done"
f _ = "Fail"

main :: Effect _
main = log $ f { type: p.type, foo: "bar" }
