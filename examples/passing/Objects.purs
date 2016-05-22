module Main where

import Prelude hiding (append)
import Control.Monad.Eff.Console (log)

test = \x -> x.foo + x.bar + 1.0

append = \o -> { foo: o.foo, bar: 1.0 }

apTest = append({foo : "Foo", baz: "Baz"})

f = (\a -> a.b.c) { b: { c: 1.0, d: "Hello" }, e: "World" }

g = (\a -> a.f { x: 1.0, y: "y" }) { f: \o -> o.x + 1.0 }

typed :: { foo :: Number }
typed = { foo: 0.0 }

test2 = \x -> x."!@#"

test3 = typed."foo"

test4 = test2 weirdObj
  where
  weirdObj :: { "!@#" :: Number }
  weirdObj = { "!@#": 1.0 }

test5 = case { "***": 1.0 } of
  { "***": n } -> n

test6 = case { "***": 1.0 } of
             { "***": n } -> n

test7 {a:    snoog , b     : blah } = blah

main = log "Done"
