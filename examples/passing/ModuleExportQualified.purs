module A (module Prelude) where
  import Prelude

module Main where
  import Control.Monad.Eff.Console
  import qualified A as B

  main = do
    print (B.show 1.0)
