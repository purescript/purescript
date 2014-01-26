module TypeDecl where

  id :: forall a. (a) -> a
  id = \x -> x

  k :: String -> Number -> String
  k = \x -> \y -> x

  iterate :: forall a. Number -> (a -> a) -> a -> a
  iterate = \n -> \f -> \a -> {
      var result = a;
      for (i <- 0 until n) {
	result = f result;
      }
      return result;
    }
    
module Main where

main = Trace.trace "Done"
