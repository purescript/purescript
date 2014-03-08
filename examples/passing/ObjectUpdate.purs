module ObjectUpdate where

  update1 = \o -> o { foo = "Foo" }

  update2 :: forall r. { foo :: String | r } -> { foo :: String | r }
  update2 = \o -> o { foo = "Foo" }

  replace = \o -> case o of
    { foo = "Foo" } -> o { foo = "Bar" }
    { foo = "Bar" } -> o { bar = "Baz" }
    o -> o

  polyUpdate :: forall a r. { foo :: a | r } -> { foo :: String | r }
  polyUpdate = \o -> o { foo = "Foo" }

  inferPolyUpdate = \o -> o { foo = "Foo" }

module Main where

import ObjectUpdate

main = Debug.Trace.trace ((update1 {foo: ""}).foo)
