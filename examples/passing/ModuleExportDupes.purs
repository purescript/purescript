module A (module Prelude) where
  import Prelude

module B (module Prelude) where
  import Prelude

module C (module Prelude, module A) where
  import Prelude
  import A

module Main where
  import Control.Monad.Eff.Console
  import A
  import B
  import C
  import Prelude

  main = do
    print (show 1.0)
