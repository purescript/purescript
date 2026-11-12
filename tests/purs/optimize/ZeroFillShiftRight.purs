module Main where

import Data.Int.Bits (zshr)

shift :: Int -> Int -> Int
shift x count = zshr x count
