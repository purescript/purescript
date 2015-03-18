module Main where

type Foo = (x :: Number | (y :: Number | (z :: Number)))
type Bar = (x :: Number, y :: Number, z :: Number)
type Baz = { w :: Number | Bar }

foo :: { | Foo }
foo = { x: 0, y: 0, z: 0 }

bar :: { | Bar }
bar = { x: 0, y: 0, z: 0 }

id' :: Object Foo -> Object Bar
id' = id

foo' :: { | Foo }
foo' = id' foo

bar' :: { | Bar }
bar' = id' bar

baz :: Baz
baz = { x: 0, y: 0, z: 0, w: 0 }

main = Debug.Trace.trace "Done"
