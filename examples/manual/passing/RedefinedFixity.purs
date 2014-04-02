module M1 where

import Prelude ()

($) f a = f a

infixr 1000 $

module M2 where

import Prelude ()

import M1

module M3 where

import Prelude ()

import M1
import M2

module Main where

main = Debug.Trace.trace "Done"
