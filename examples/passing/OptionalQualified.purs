module Main where

-- qualified import with the "qualified" keyword
import qualified Prelude as P

-- qualified import without the "qualified" keyword
import Control.Monad.Eff.Console as Console

bind = P.bind

main = do
  message <- P.return "success!"
  Console.log message
