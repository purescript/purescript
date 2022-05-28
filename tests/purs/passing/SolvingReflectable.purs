module Main where

import Prelude

import Data.Ordering (Ordering(..))
import Data.Reflectable (reflectType)
import Effect.Console (log)
import Prim.Boolean (True, False)
import Prim.Ordering (LT, EQ, GT)
import Type.Proxy (Proxy(..))

refInt :: Proxy 42
refInt = Proxy

refIntPass :: Boolean
refIntPass = reflectType refInt == 42

refString :: Proxy "PureScript"
refString = Proxy

refStringPass :: Boolean
refStringPass = reflectType refString == "PureScript"

refBooleanT :: Proxy True
refBooleanT = Proxy

refBooleanF :: Proxy False
refBooleanF = Proxy

refBooleanPass :: Boolean
refBooleanPass = reflectType refBooleanT == true && reflectType refBooleanF == false

refOrderingLT :: Proxy LT
refOrderingLT = Proxy

refOrderingEQ :: Proxy EQ
refOrderingEQ = Proxy

refOrderingGT :: Proxy GT
refOrderingGT = Proxy

refOrderingPass :: Boolean
refOrderingPass =
  reflectType refOrderingLT == LT
  && reflectType refOrderingEQ == EQ
  && reflectType refOrderingGT == GT

main = do
  when (refIntPass && refStringPass && refBooleanPass && refOrderingPass) $
    log "Done"
