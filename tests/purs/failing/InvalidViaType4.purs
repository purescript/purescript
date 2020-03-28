-- @shouldFailWith InvalidViaType
module Main where

class C a

data D = D

derive via "" instance cd :: C D
