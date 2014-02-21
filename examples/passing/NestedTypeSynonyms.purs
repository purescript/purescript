module NestedTypeSynonyms where

  type X = String
  type Y = X -> X

  fn :: Y
  fn a = a

module Main where

  import Prelude

  main = Debug.Trace.print (NestedTypeSynonyms.fn "Done")
