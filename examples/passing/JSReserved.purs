module Main where

  import Prelude
  import Data.Array

  yield = 0
  member = 1
  
  public = \return -> return
  
  this catch = catch

  main = Debug.Trace.print [ yield
                     , member
                     , this 1
                     , public 1 ]
