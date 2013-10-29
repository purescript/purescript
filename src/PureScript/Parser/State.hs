-----------------------------------------------------------------------------
--
-- Module      :  PureScript.Parser.State
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

module PureScript.Parser.State where

import PureScript.Names
import PureScript.Declarations

import qualified Text.Parsec as P
import qualified Data.Map as M

data ParseState = ParseState
  { indentationLevel :: P.Column
  , fixities :: M.Map String Fixity } deriving Show


