module Assign where

  sum = \n -> {
      var x = 0;
      for (i <- 0 until n) {
	x = x + i;
      }
      return x;
    }
    
module Main where

main = Trace.trace "Done"
