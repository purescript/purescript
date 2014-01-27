module Assign where

  sum = \n -> {
      var x = 0;
      for (i <- 0 until n) {
        x = x + i;
      }
      return x;
    }

  arr = [1, 2]

  foo = {
    var x = 0; -- mutable
    var y = 1; -- immutable

    -- x should not be inlined since x is mutable
    while (x < 10) {
      x = x + 1;
    }

    var z1 = arr !! x; -- immutable
    var b = (\x -> z1) 2; -- z1 should not be inlined since x is bound

    var z2 = arr !! x; -- immutable
    var z3 = arr !! x; -- immutable
    var a = (\z2 -> z2) 2; -- z3 should not be inlined since z2 is bound

    -- y should be inlined
    -- z3 should be inlined
    return x + y + z1 + z2 + z3 + a + b;
  }

module Main where

main = Trace.trace "Done"
