-- |
-- The data type of compiler options
--
module Language.PureScript.Options where

import Prelude.Compat

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
    -- Generate soure maps
  , optionsSourceMaps :: Bool
    -- |
    -- Dump CoreFn
  , optionsDumpCoreFn :: Bool
  } deriving Show

-- |
-- Default make options
defaultOptions :: Options
defaultOptions = Options False False Nothing False False False False False
