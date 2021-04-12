-- @shouldWarnWith ShadowedName
module Main where

import Prelude

import Effect (Effect)

test :: forall m. Monad m => Int -> m Unit
test x =
  let _ = x in -- don't mark x unused
  let x = unit
  in pure x

main :: Effect Unit
main = test 42

