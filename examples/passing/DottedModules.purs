module Some.Module.Name where

  class Foo a where
    foo :: a -> a
  
  x = "Done"

module Main where

  import Prelude
  import Some.Module.Name
  
  instance Some.Module.Name.Foo String where
    foo s = s

  main = Trace.print $ foo x
