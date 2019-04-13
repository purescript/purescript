module Main where

import Prelude as P
import Effect (Effect)
import Effect.Console (log)

class P.Show a <= Show a where
  id :: a -> a

instance showString :: Show String where
  id x = x

main :: Effect P.Unit
main = log (id "Done")
