-- @shouldFailWith FloatingViaTypeVariables
module Main where

class C a

data D = D
data V a = V

derive via (V a) instance cd :: C D
