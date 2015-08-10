-- @shouldFailWith KindsDoNotUnify
module Main where

import Prelude

data Foo = Bar
type Baz = { | Foo }

main = Control.Monad.Eff.Console.log "Done"
