module Main where

(<!!>) :: Number -> String -> String
num <!!> str = show num ++ str

(<!!!>) :: Number -> String -> String
num <!!!> str | num == 0 = (<!!>) 100 str
num <!!!> str | otherwise = num <!!> str
 
main = Debug.Trace.trace "Done"
