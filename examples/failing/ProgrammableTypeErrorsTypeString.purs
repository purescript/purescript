-- @shouldFailWith NoInstanceFound

module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)

newtype MyType a = MyType a

instance cannotShowFunctions :: Fail ("Don't want to show " <> TypeString (MyType a) <> " because.") => Show (MyType a) where
  show _ = "unreachable"

infixl 6 type TypeConcat as <>

main :: Eff _ _
main = do
  log $ show (MyType 2)
