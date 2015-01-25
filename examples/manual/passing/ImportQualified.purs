module M1 where

  trace x = x

module Main where

  import Prelude
  import Control.Monad.Eff
  import M1
  import qualified Debug.Trace as T

  main = T.trace (trace "Done")
