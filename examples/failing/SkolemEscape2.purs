-- @shouldFailWith EscapedSkolem
module Main where

import Prelude
import Effect
import Control.Monad.ST

test _ = do
  r <- runST (newSTRef 0)
  pure 0
