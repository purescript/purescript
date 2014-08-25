module Main where

import Control.Monad.Eff

bug :: Number -> Number -> Number
bug a b = 0 - (a - b)

foreign import explode 
  "function explode() {\
  \  throw new Error('Assertion failed!');\
  \}":: forall eff a. Eff eff a

main = case bug 0 2 of
  2 -> Debug.Trace.trace "Done!"
  _ -> explode
