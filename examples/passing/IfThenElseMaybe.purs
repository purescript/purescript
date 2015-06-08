module Main where

import Prelude

data Maybe a = Nothing | Just a

test1 = if true then Just 10 else Nothing

test2 = if true then Nothing else Just 10

main = Debug.Trace.trace "Done"
