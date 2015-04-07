module Main where

(<!!>) :: Number -> Number -> String -> String
num1 num2 <!!> str = show num1 ++ str

main = Debug.Trace.trace "Done"
