-- @shouldFailWith InvalidViaType
module Main where

class C a

data D = D

derive via _ instance cd :: C D
