module Shadowed where

import Prelude

a = [1, 2]

foo = {
    var x = a !! 0;
    var a = 0;

    a = a + 1;

    return x + a;
  }
