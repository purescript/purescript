module Foreach where

  sum = \ns -> {
      var total = 0;
      foreach (n in ns) {
	total = total + n;
      }
      return total;
    }
