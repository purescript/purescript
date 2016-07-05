module Main where

import Prelude
import Control.Monad.Eff.Console (log)

update1 = \o -> o { foo = "Foo" }

update2 :: forall r. { foo :: String | r } -> { foo :: String | r }
update2 = \o -> o { foo = "Foo" }

replace = \o -> case o of
  { foo: "Foo" } -> o { foo = "Bar" }
  { foo: "Bar" } -> o { bar = "Baz" }
  o -> o

polyUpdate :: forall a r. { foo :: a | r } -> { foo :: String | r }
polyUpdate = \o -> o { foo = "Foo" }

inferPolyUpdate = \o -> o { foo = "Foo" }

main = do
  log ((update1 {foo: ""}).foo)
  log "Done"
