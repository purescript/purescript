-- @shouldFailWith InvalidViaKind
module Main where

class C a

data V a = V

instance cv :: C (V a)

data D = D

derive via V instance cd :: C D
