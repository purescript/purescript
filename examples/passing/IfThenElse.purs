module IfThenElse where

  import Prelude

  testIf = 
    {
      var x = 0;
      if (x == 0) { 
	x = x + 1; 
      }
      return x;
    }

  testIfElse =
    {
      var x = 0;
      if (x == 0) {
	x = x + 1;
      } else {
	x = x - 1;
      }
      return x;
    }

  testIfElseIf = \x ->
    {
      var y = x;
      while (y > 1) {
	if (y % 3 == 0) {
	  y = y / 3;
	} else if (y % 3 == 1) {
	  y = (y - 1) % 3;
	} else {
	  y = y * 4 + 1;
	}
      }
      return y;
    }
    
module Main where

main = Trace.trace "Done"
