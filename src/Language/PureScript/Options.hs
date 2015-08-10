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
-- The data type of compiler options
--
-----------------------------------------------------------------------------

module Language.PureScript.Options where

-- |
-- The data type of compiler options
--
data Options = Options {
    -- |
    -- Disable tail-call elimination
    optionsNoTco :: Bool
    -- |
    -- Disable inlining of calls to return and bind for the Eff monad
  , optionsNoMagicDo :: Bool
    -- |
    -- When specified, checks the type of `main` in the module, and generate a call to run main
    -- after the module definitions.
  , optionsMain :: Maybe String
    -- |
    -- Skip all optimizations
  , optionsNoOptimizations :: Bool
    -- |
    -- Verbose error message
  , optionsVerboseErrors :: Bool
    -- |
    -- Remove the comments from the generated js
  , optionsNoComments :: Bool
    -- |
    -- The path to prepend to require statements
  , optionsRequirePath :: Maybe FilePath
  } deriving Show

-- |
-- Default make options
defaultOptions :: Options
defaultOptions = Options False False Nothing False False False Nothing
