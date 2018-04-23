-- @shouldFailWith EscapedSkolem
module Main where

import Prelude
import Effect
import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef

test _ = do
  r <- pure (ST.run (STRef.new 0))
  pure r
