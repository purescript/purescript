module A (module Prelude) where
  import Prelude

module Main where
  import Control.Monad.Eff.Console
  import A hiding (module Prelude)

  otherwise = false

  main = do
    print "1.0"
