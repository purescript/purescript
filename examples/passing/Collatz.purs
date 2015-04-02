module Main where

import Control.Monad.Eff
import Control.Monad.ST

foreign import jsMod
  """
  function jsMod(x) {
    return function (y) {
      return x % y;
    };
  }
  """ :: Number -> Number -> Number

infixl 7 %
(%) = jsMod

collatz :: Number -> Number
collatz n = runPure (runST (do
  r <- newSTRef n
  count <- newSTRef 0
  untilE $ do
    modifySTRef count $ (+) 1
    m <- readSTRef r
    writeSTRef r $ if m % 2 == 0 then m / 2 else 3 * m + 1
    return $ m == 1
  readSTRef count))

main = Debug.Trace.print $ collatz 1000
