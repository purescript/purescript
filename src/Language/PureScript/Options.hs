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

{-# LANGUAGE GADTs, DataKinds, StandaloneDeriving #-}

module Language.PureScript.Options where

-- |
-- Indicates the mode of the compiler. Lifted using DataKinds to refine the Options type.
--
data Mode = Compile | Make

-- |
-- Per-mode options
--
data ModeOptions mode where
  CompileOptions :: String -> [String] -> [String] -> ModeOptions Compile
  MakeOptions :: Maybe FilePath -> ModeOptions Make

browserNamespace :: ModeOptions Compile -> String
browserNamespace (CompileOptions ns _ _) = ns

entryPointModules :: ModeOptions Compile -> [String]
entryPointModules (CompileOptions _ ms _) = ms

codeGenModules :: ModeOptions Compile -> [String]
codeGenModules (CompileOptions _ _ ms) = ms

requirePath :: ModeOptions Make -> Maybe FilePath
requirePath (MakeOptions mp) = mp

deriving instance Show (ModeOptions mode)

-- |
-- The data type of compiler options
--
data Options mode = Options {
    -- |
    -- Disable tail-call elimination
    --
    optionsNoTco :: Bool
    -- |
    -- Disable inlining of calls to return and bind for the Eff monad
    --
  , optionsNoMagicDo :: Bool
    -- |
    -- When specified, checks the type of `main` in the module, and generate a call to run main
    -- after the module definitions.
    --
  , optionsMain :: Maybe String
    -- |
    -- Skip all optimizations
    --
  , optionsNoOptimizations :: Bool
    -- |
    -- Verbose error message
    --
  , optionsVerboseErrors :: Bool
    -- |
    -- Remove the comments from the generated js
  , optionsNoComments :: Bool
    -- |
    -- Specify the namespace that PureScript modules will be exported to when running in the
    -- browser.
    --
  , optionsAdditional :: ModeOptions mode
  } deriving Show

-- |
-- Default compiler options
--
defaultCompileOptions :: Options Compile
defaultCompileOptions = Options False False Nothing False False False (CompileOptions "PS" [] [])

-- |
-- Default make options
--
defaultMakeOptions :: Options Make
defaultMakeOptions = Options False False Nothing False False False (MakeOptions Nothing)
