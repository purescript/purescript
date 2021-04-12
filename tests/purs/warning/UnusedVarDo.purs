-- @shouldWarnWith UnusedName
-- @shouldWarnWith UnusedName
-- @shouldWarnWith UnusedName
-- @shouldWarnWith UnusedName
module Main where

import Prelude
import Data.Maybe (Maybe)

unusedDoBinding :: Maybe Int
unusedDoBinding = do
  unusedDoBind <- pure 42
  pure 17

usedDoBinding :: Maybe Int
usedDoBinding = do
  fine <- pure 42
  let alsoFine = 1
  pure $ fine + alsoFine


unusedDoLetBinding :: Maybe Int
unusedDoLetBinding = do
  let unusedDoLet = 42
  pure 17

unusedAdoBinding :: Maybe Int
unusedAdoBinding = ado
  unusedAdoBind <- pure 42
  in 17

unusedAdoLetBinding :: Maybe Int
unusedAdoLetBinding = ado
  let unusedAdoLet = 42
  in 17