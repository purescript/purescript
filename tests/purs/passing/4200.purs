module Main where

import Data.Newtype (class Newtype)
import Effect.Console (log)
import Lib (T, TAlias)

newtype NewA a = NewA (TAlias Int)

derive instance Newtype (NewA a) _

main = log "Done"
