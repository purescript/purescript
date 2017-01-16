module Main where

import Control.Monad.Eff.Console (log)

main = log ({hasOwnProperty: "Hi"} {hasOwnProperty = "Done"}).hasOwnProperty
