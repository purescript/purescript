-- @shouldFailWith UnknownName
module Main where

import Prelude

-- The 'Cons' class is not imported here, so we should not be able to refer to
-- it in the module.
x :: Cons "hello" Int () ("hello" :: Int)
  => Unit
x = unit

