module Test.Main where

import Prelude
import Effect (Effect)
import Test.GenericDeriving as GenericDeriving

main :: Effect Unit
main = GenericDeriving.main
