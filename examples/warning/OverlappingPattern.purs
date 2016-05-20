-- @shouldWarnWith OverlappingPattern
-- @shouldWarnWith OverlappingPattern
module Main where

data X = A | B

pat1 :: X -> Boolean
pat1 A = true
pat1 A = true
pat1 B = false

pat2 :: X -> Boolean
pat2 A = true
pat2 _ = false
pat2 B = false
