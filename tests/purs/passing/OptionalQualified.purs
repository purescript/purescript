module Main where

import Prelude as P

-- qualified import without the "qualified" keyword
import Effect.Console as Console

bind = P.bind

main = do
  message <- P.pure "Done"
  Console.log message
