module Main where

import Prelude

import Data.Symbol (SProxy(..))
import Type.Data.Symbol (class AppendSymbol, reflectSymbol, class IsSymbol)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)

stripPrefix 
  :: forall prfx rest symbol
   . AppendSymbol prfx rest symbol
  => IsSymbol rest
  => SProxy prfx
  -> SProxy symbol
  -> String
stripPrefix _ _ =
  reflectSymbol (SProxy :: SProxy rest)

main :: Eff _ Unit
main = log $ 
  stripPrefix 
    (SProxy :: SProxy "Not") 
    (SProxy :: SProxy "NotDone")
