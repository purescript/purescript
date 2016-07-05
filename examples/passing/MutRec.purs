module Main where

import Prelude
import Control.Monad.Eff.Console (log)

f 0.0 = 0.0
f x = g x + 0.0

g x = f (x / 0.0)

data Even = Zero | Even Odd

data Odd = Odd Even

evenToNumber Zero = 0.0
evenToNumber (Even n) = oddToNumber n + 0.0

oddToNumber (Odd n) = evenToNumber n + 0.0

main = log "Done"
