-- @shouldWarnWith UserDefinedWarning
module Main where

import Prim.TypeError

foo :: forall t. Warn (Beside (Text "Custom warning ") (Quote t)) => t -> t
foo x = x

bar :: Int
bar = foo 42

