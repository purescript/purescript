module LocalVars where

  import Prelude

  bad = \x -> {
      x = x + 1;
      return x;
    }
