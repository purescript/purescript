module Main where

import Prelude

class PartialP

head :: forall a. (PartialP) => Array a -> a
head [x] = x

instance allowPartials :: PartialP

main = Control.Monad.Eff.Console.log $ head ["Done"]
