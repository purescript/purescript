-----------------------------------------------------------------------------
--
-- Module      :  PureScript.Declarations
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module PureScript.Declarations (
    Declaration (..),
    DataConstructors (..)
) where

import PureScript.Values
import PureScript.Types

data Declaration
  = DataDeclaration DataConstructors
  | TypeDeclaration String Type
  | ValueDeclaration String Value deriving Show

data DataConstructors = DataConstructors
  { typeConstructorName :: String
  , typeArguments :: [String]
  , dataConstructors :: [(String, Type)]
  } deriving Show
