module Test where

data Z = Z String

runZ :: Z -> String
runZ (Z s) = s
