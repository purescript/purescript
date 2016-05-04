-- @shouldFailWith ImportHidingModule
module Main where

import B hiding (module A)
