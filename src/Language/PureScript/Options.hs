-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Options
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

module Language.PureScript.Options where

data Options = Options
  { optionsTco :: Bool
  , optionsPerformRuntimeTypeChecks :: Bool
  , optionsMagicDo :: Bool
  } deriving Show

defaultOptions :: Options
defaultOptions = Options False False False
