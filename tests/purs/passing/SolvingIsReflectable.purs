module Main where

import Prelude

import Data.Reflectable (reflectType)
import Effect.Console (log)

data Proxy :: forall k. k -> Type
data Proxy n = Proxy

refInt :: Proxy 42
refInt = Proxy

refIntPass :: Boolean
refIntPass = reflectType refInt == 42

refString :: Proxy "PureScript"
refString = Proxy

refStringPass :: Boolean
refStringPass = reflectType refString == "PureScript"

main = do
  when (refIntPass && refStringPass) $
    log "Done"
