module Main where

import Effect.Console (log)
import Prim.Int (class DivMod)

data Proxy q r = Proxy

odd :: forall q r. DivMod 10 3 q r => Proxy q r
odd = Proxy

odd' :: Proxy 3 1
odd' = odd

even :: forall q r. DivMod 10 2 q r => Proxy q r
even = Proxy

even' :: Proxy 5 0
even' = even

oddNeg :: forall q r. DivMod 10 (-3) q r => Proxy q r
oddNeg = Proxy

oddNeg' :: Proxy (-4) (-2)
oddNeg' = oddNeg

evenNeg :: forall q r. DivMod 10 (-2) q r => Proxy q r
evenNeg = Proxy

evenNeg' :: Proxy (-5) 0
evenNeg' = evenNeg

oddNum :: forall n. DivMod n 3 3 1 => Proxy n n
oddNum = Proxy

oddNum' :: Proxy 10 10
oddNum' = oddNum

evenNum :: forall n. DivMod n 2 5 0 => Proxy n n
evenNum = Proxy

evenNum' :: Proxy 10 10
evenNum' = evenNum

main = log "Done"
