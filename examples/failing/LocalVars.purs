module LocalVars where

  bad = \x -> {
      x = x + 1;
      return x;
    }
