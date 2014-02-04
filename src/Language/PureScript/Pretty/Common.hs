-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Pretty.Common
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Common pretty-printing utility functions
--
-----------------------------------------------------------------------------

module Language.PureScript.Pretty.Common where

-- |
-- Wrap a string in parentheses
--
parens :: String -> String
parens s = ('(':s) ++ ")"
