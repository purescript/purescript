module JSReserved where

  import Prelude

  yield = 0
  member = 1
  
  this = {
    var function = 0;
    for (i <- 0 until 10) {
      function = function + i;
    }
    return function;
  }
  
  catch = {
    var for = 0;
    while (for > 0) {
      for = for - 1;
    }
    return for;
  }
  
  instanceof = {
    var var = 1;
    var = var + 1;
    return var;
  }
  
  public = \return -> return
  
module Main where

  import Prelude
  import Arrays
  import JSReserved

  main = Trace.print [ yield
                     , member
                     , this
                     , catch
                     , instanceof
                     , public 1 ]