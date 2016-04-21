module Main where

import Prelude
import Control.Monad.Eff.Console

main = logShow (const unit $ "Hello world")
