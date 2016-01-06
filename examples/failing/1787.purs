-- @shouldFailWith ErrorParsingModule
module Main where

main = do
  f $ \x ->
    let y = z
        z = 0

g :: Int
g = 3
