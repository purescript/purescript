module Main where

import Prelude
import Control.Monad.ST (ST)
import Control.Monad.ST.Uncurried (STFn1, STFn2, mkSTFn1, mkSTFn2, runSTFn1, runSTFn2)

mySTFn1 :: forall r. STFn1 Int r Int
mySTFn1 = mkSTFn1 \a -> pure (a + 1)

mySTFn2 :: forall r. STFn2 Int Int r Int
mySTFn2 = mkSTFn2 \a b -> pure (a + b)

myInt1 :: forall r. ST r Int
myInt1 = runSTFn1 mySTFn1 0

myInt2 :: forall r. ST r Int
myInt2 = runSTFn2 mySTFn2 0 1
