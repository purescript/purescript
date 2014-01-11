module Tests where

import JQuery

test = eff do
  b <- body
  div <- create "<div>"
  { color: "red" } `css` div
  appendText "Hello World" div
  b `append` div
