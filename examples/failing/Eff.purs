module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.ST
import Debug.Trace

test = pureST (do
         ref <- newSTRef 0
         trace "ST"
         modifySTRef ref $ \n -> n + 1
         readSTRef ref)
