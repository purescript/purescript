module Main where

  import Prelude

  yield = "test"
  member = "test"
  
  fortest = {
    var function = 0;
    for (i <- 0 until 10) {
      function = function + i;
    }
    return function;
  }
  
  whiletest = {
    var for = 0;
    while (for > 0) {
      for = for - 1;
    }
    return for;
  }
  
  vartest = {
    var var = 1;
    var = var + 1;
    return var;
  }

  main = Trace.trace "Done"