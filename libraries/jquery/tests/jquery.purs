module Main where

import Prelude
import Eff
import Trace
import JQuery

main = do
  -- Get the document body
  b <- body

  -- Create a text box
  div <- create "<div>"
  input <- create "<input>"
  "Your Name: " `appendText` div
  input `append` div
  div `append` b

  -- Create a paragraph to display a greeting
  greeting <- create "<p>"
  { color: "red" } `css` greeting
  greeting `append` b

  -- Listen for change events on the text box
  flip (on "change") input $ do
    name <- getValue input
    trace $ "Name changed to " ++ name
    setText ("Hello, " ++ name) greeting
