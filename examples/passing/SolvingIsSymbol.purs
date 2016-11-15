module Main where

import Prelude
import Data.Symbol
import Control.Monad.Eff
import Control.Monad.Eff.Console

main = do
  let lit = reflectSymbol (SProxy :: SProxy "literal")
  when (lit == "literal") (log "Done")
