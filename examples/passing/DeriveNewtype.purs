module Main where

import Control.Monad.Eff.Console (log)

import Data.Newtype

newtype Test = Test String

derive instance newtypeTest :: Newtype Test _

t :: Test
t = wrap "hello"

a :: String
a = unwrap t

newtype First a = First a

derive instance newtypeFirst :: Newtype (First b) _

f :: First Int
f = wrap 1

i :: Int
i = unwrap f

main = log "Done"
