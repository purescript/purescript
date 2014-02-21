module Main where

import Prelude
import Control.Monad.Eff
import Data.Array
import Global

main = do
  let test1 = concat [1, 2, 3] [4, 5, 6]
  Debug.Trace.print test1
