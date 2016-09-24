module Main where

import Control.Monad.Eff.Console (log)

import Data.Newtype

newtype Test = Test String

derive instance newtypeTest :: Newtype Test _

t :: Test
t = wrap "hello"

a :: String
a = unwrap t

main = log "Done"
