module Main where

import Eff
import JQuery

main = eff do
  b <- body
  div <- create "<div>"
  { color: "red" } `css` div
  appendText "Hello World" div
  div `append` b
