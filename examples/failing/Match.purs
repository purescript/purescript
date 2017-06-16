-- @shouldFailWith NoInstanceFound
module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Symbol

newtype Lowercase = Lowercase String

lit :: forall s. IsSymbol s => Match "^[^A-Z]*$" s => SProxy s -> Lowercase
lit = Lowercase <<< reflectSymbol

main :: Eff (console :: CONSOLE) Unit
main = do
  _ <- pure <<< lit $ SProxy :: SProxy "foo BAR baz"
  log "Done"
