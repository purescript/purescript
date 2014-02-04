module OptimizerBug where

import Prelude

x = 1 + y

y = x
    
module Main where

main = Trace.trace "Done"
