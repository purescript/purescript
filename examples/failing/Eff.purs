-- @shouldFailWith TypesDoNotUnify
module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.ST
import Control.Monad.Eff.Console

test = pureST (do
         ref <- newSTRef 0
         log "ST"
         modifySTRef ref $ \n -> n + 1
         readSTRef ref)
