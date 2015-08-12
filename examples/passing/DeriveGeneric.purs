module Main where

import Prelude

import Data.Generic

data X = X String Int Number

instance genericX :: Generic X

main = Control.Monad.Eff.Console.log (gShow (X "Hello" 1 2.0))
