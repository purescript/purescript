-- @shouldFailWith NoInstanceFound

module Main where

import Prelude
import Prim.TypeError
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)

newtype MyType a = MyType a

instance cannotShowFunctions ::
  Fail ( Text "Don't want to show " <>
         Quote (MyType a) <>
         Text " because."
       ) => Show (MyType a)
  where
    show _ = "unreachable"

infixl 6 type Beside as <>

main :: Eff _ _
main = do
  log $ show (MyType 2)
