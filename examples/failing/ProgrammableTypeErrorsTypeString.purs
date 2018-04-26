-- @shouldFailWith NoInstanceFound

module Main where

import Prelude
import Prim.TypeError
import Effect (Effect)
import Effect.Console (log)

newtype MyType a = MyType a

instance cannotShowFunctions ::
  Fail ( Text "Don't want to show " <>
         Quote (MyType a) <>
         Text " because."
       ) => Show (MyType a)
  where
    show _ = "unreachable"

infixl 6 type Beside as <>

main :: Effect Unit
main = do
  log $ show (MyType 2)
