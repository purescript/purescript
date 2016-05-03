module Main where

import Prelude
import Control.Monad.Eff.Console (log)

class PartialP

head :: forall a. (PartialP) => Array a -> a
head [x] = x

instance allowPartials :: PartialP

main = log $ head ["Done"]
