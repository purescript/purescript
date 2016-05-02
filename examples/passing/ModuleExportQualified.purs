module A (module Prelude) where
  import Prelude

module Main where
  import Control.Monad.Eff.Console
  import A as B

  main = do
    logShow (B.show 1.0)
