module Main where

getX = _.x

point = { x: 1, y: 0 }

main = do
  Debug.Trace.print $ getX point
  Debug.Trace.trace $ _.value { value: "Done!" }
