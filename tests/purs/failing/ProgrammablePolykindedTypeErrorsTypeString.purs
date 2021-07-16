-- @shouldFailWith NoInstanceFound

module Main where

import Prelude
import Prim.TypeError
import Effect (Effect)
import Effect.Console (log)

data Maybe :: forall k. k -> Type
data Maybe a

foreign import data Nothing :: forall k. Maybe k
foreign import data Just :: forall k. k -> Maybe k

someString :: Fail (Text "Don't want to show " <> Quote (Just String) <> Text " because.") => String
someString = "someString"

infixl 6 type Beside as <>

main :: Effect Unit
main = do
  log someString
