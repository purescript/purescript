module Main where

  import Prelude
  import Prelude.Unsafe
  import Control.Monad.Eff
  import Debug.Trace
  import Assert

  fiblike :: Int -> String
  fiblike 0 = "0"
  fiblike 1 = "1"
  fiblike n = reflectParameterName fiblike

  -- This test ensures that the rendered javascript function uses a "pretty"
  -- variable name taken from the purescript function binding: in this case "n"
  --
  main = do
    assert (fiblike 10 == "n")
    trace "Done"
