-- @shouldFailWith TypesDoNotUnify

module Main where

import Prelude
import Control.Monad.Eff (Eff)

a :: proxy "a"
a = @"b"

main :: Eff _ _
main = pure unit
