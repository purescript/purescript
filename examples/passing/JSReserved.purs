module Main where

  import Prelude

  const = "test"
  member = "test"
  
  fortest = {
    var return = 0;
    for (i <- 0 until 10) {
      return = return + i;
    }
    return return;
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