module JSReserved where

  import Prelude

  yield = 0
  member = 1
  
  public = \return -> return
  
  this catch = catch

module Main where

  import Prelude
  import Data.Array
  import JSReserved

  main = Debug.Trace.print [ yield
                     , member
                     , this 1
                     , public 1 ]
