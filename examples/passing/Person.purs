module Main where

import Prelude
import Control.Monad.Eff.Console (log)

data Person = Person { name :: String, age :: Number }

showPerson :: Person -> String
showPerson = \p -> case p of
  Person o -> o.name <> ", aged " <> show o.age

main = log "Done"
