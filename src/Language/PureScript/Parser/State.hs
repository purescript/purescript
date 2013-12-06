-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Parser.State
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

module Language.PureScript.Parser.State where

import qualified Text.Parsec as P

data ParseState = ParseState
  { indentationLevel :: P.Column } deriving Show


