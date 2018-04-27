module Main where

import Effect.Console (log)

main = log ({hasOwnProperty: "Hi"} {hasOwnProperty = "Done"}).hasOwnProperty
