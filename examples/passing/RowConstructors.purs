module Main where

import Prelude

type Foo = (x :: Number | (y :: Number | (z :: Number)))
type Bar = (x :: Number, y :: Number, z :: Number)
type Baz = { w :: Number | Bar }

foo :: { | Foo }
foo = { x: 0.0, y: 0.0, z: 0.0 }

bar :: { | Bar }
bar = { x: 0.0, y: 0.0, z: 0.0 }

id' :: Object Foo -> Object Bar
id' = id

foo' :: { | Foo }
foo' = id' foo

bar' :: { | Bar }
bar' = id' bar

baz :: Baz
baz = { x: 0.0, y: 0.0, z: 0.0, w: 0.0 }

type Quux r = (q :: Number | r)
type Norf r = (q' :: Number | Quux r)

quux :: { f :: { | Foo } | Quux Bar }
quux = { f: foo', x: 0.0, y: 0.0, z: 0.0, q: 0.0 }

quux' :: { | Norf Bar }
quux' = { x: 0.0, y: 0.0, z: 0.0, q: 0.0, q': 0.0 }

wildcard :: { w :: Number | _ } -> Baz
wildcard { w: w } = { x: w, y: w, z: w, w: w }

wildcard' :: { | Quux _ } -> Number
wildcard' { q: q } = q

main = Control.Monad.Eff.Console.log "Done"
