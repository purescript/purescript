-- See https://github.com/purescript/purescript/issues/606
module Main where

import Prelude

o :: { type :: String }
o = { type: "o" }

p :: { type :: String }
p = o { type = "p" }

f :: forall r. { type :: String | r } -> String
f { type = "p" } = "Done"

main = Control.Monad.Eff.Console.log $ f { type: p.type, foo: "bar" }
