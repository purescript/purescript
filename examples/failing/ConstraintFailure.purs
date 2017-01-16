-- @shouldFailWith NoInstanceFound

module Main where

import Prelude

data Foo = Bar
       
spin :: forall a. a -> Foo
spin x = Bar

main = show <<< spin
     
