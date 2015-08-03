-- @shouldFailWith EscapedSkolem
module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.ST

test _ = do
  r <- runST (newSTRef 0)
  return 0
