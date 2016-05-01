module A (module Prelude) where
  import Prelude

module Main where
  import Control.Monad.Eff.Console
  import A

  main = do
    logShow (show 1.0)
