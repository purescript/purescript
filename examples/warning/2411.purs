-- @shouldWarnWith ShadowedName
module Main where

import Prelude

import Control.Monad.Eff (Eff)

test :: forall m. Monad m => Int -> m Unit
test x =
  let x = unit
  in pure x

main :: Eff () Unit
main = test 42

