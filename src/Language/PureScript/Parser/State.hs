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
-- State for the parser monad
--
-----------------------------------------------------------------------------

module Language.PureScript.Parser.State where

import qualified Text.Parsec as P

-- |
-- State for the parser monad
--
data ParseState = ParseState {
    -- |
    -- The most recently marked indentation level
    --
    indentationLevel :: P.Column
  } deriving Show


