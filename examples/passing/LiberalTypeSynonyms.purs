module Main where

import Prelude

type Reader = (->) String

foo :: Reader String
foo s = s

type AndFoo r = (foo :: String | r)

getFoo :: forall r. Prim.Object (AndFoo r) -> String
getFoo o = o.foo

type F r = { | r } -> { | r }

f :: (forall r. F r) -> String
f g = case g { x: "Hello" } of
        { x = x } -> x

main = Control.Monad.Eff.Console.log "Done"
