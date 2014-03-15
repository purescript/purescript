module Main where

  import Prelude
  import Data.Lazy
  import Data.Array
  import Control.Monad
  import Control.Monad.Eff
  import Control.Monad.Eff.Error

  foreign import error "function error(s) { throw s; }" :: forall a. String -> a

  suspension1 = lazy (\_ -> 1)
  suspension2 = lazy (\_ -> error "should not be evaluated") :: Lazy String

  main = do
    if (force suspension1) == 1
      then return "OK"
      else throwError $ "Expected 1 but " ++ show (force suspension1)
    if length [suspension2] == 1
      then return "OK"
      else throwError "Not done"

    Debug.Trace.trace "Done"
