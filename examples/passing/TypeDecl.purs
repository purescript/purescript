module TypeDecl where

  import Prelude

  k :: String -> Number -> String
  k x y = x

  iterate :: forall a. Number -> (a -> a) -> a -> a
  iterate 0 f a = a
  iterate n f a = iterate (n - 1) f (f a)
    
module Main where

main = Debug.Trace.trace "Done"
