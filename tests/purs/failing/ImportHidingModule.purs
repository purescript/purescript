-- @shouldFailWith  ErrorParsingModule
module Main where

import B hiding (module A)
