module Main where

import Prelude

ints = [{f: (+)}, {f: (-)}] <#> \x -> x.f 1 2

main = Control.Monad.Eff.Console.print ints
