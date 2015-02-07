module Main where

getX = (.x)

point = { x: 1, y: 0 }

main = do
  Debug.Trace.print $ getX point
  Debug.Trace.trace $ (." 123 string Prop Name ") { " 123 string Prop Name ": "OK" }
  Debug.Trace.trace $ ((.x) >>> (.y)) { x: { y: "Nested" } }
  Debug.Trace.trace $ (.value) { value: "Done!" }
