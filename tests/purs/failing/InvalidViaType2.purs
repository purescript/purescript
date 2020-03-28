-- @shouldFailWith InvalidViaType
module Main where

class C a

data D = D

derive via a instance cd :: C D
