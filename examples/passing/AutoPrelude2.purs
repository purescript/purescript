module Main where

import Prelude
import qualified Prelude as P
import Debug.Trace

f :: forall a. a -> a
f = P.id

main = P.($) trace ((f P.<<< f) "Done")
