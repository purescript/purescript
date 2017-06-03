-- @shouldFailWith TypesDoNotUnify

module Main where

import Prelude
import Control.Monad.Eff (Eff)

a :: @"a"
a = @Int

main :: Eff _ _
main = pure unit
