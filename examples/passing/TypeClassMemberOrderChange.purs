module Main where

import Prelude
import Control.Monad.Eff.Console (log)

class Test a where
  fn :: a -> a -> a
  val :: a

instance testBoolean :: Test Boolean where
  val = true
  fn x y = y

main = do
  log (show (fn true val))
  log "Done"
