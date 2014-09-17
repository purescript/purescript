module Main where

data Maybe a = Nothing | Just a

test = if true then Just 10 else Nothing

main = Debug.Trace.trace "Done"
