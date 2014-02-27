module EmptyTypeClass where

class Partial

head :: forall a. (Partial) => [a] -> a
head (x:xs) = x

module AllowPartialFns where

instance EmptyTypeClass.Partial

module Main where

import Prelude
import AllowPartialFns
import EmptyTypeClass

main = Debug.Trace.trace $ head ["Done"]
