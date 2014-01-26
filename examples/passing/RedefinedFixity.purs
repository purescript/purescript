module M1 where

($) f a = f a

infixr 1000 $

module M2 where

import M1

module M3 where

import M1
import M2
    
module Main where

main = Trace.trace "Done"
