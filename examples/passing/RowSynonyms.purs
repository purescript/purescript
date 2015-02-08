module Main where

import Control.Monad.Eff
import Control.Monad.ST

type State bindings =
  {
    bindings :: {addition :: Number | bindings},
    other :: String
  }

type MyBindings = (test :: Number)

data Shadow bindings = Shadow String

shadows :: Shadow (Object MyBindings)
shadows = Shadow "uhh"

main :: Eff () Unit
main = withIt
          shadows
            \ bindings -> do
                let state =
                  {
                      bindings : bindings,
                      other : "Test"
                  }
                runST do
                  stRef <- newSTRef state
                  handleKeyD stRef
                  return unit


withIt :: forall bindings eff a. Shadow (Object bindings) ->
                ({addition :: Number | bindings} -> Eff eff a) -> Eff eff a
withIt (Shadow str) success = do
  b <- withBindings
  success (b{addition = 1})

foreign import withBindings
"""
        function withBindings() {}
""" :: forall eff bindings. Eff eff bindings

handleKeyD :: forall h eff. STRef h (State MyBindings) -> Eff (st :: ST h | eff) Unit
handleKeyD state = return unit
