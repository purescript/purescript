-- | The data type of compiler options
module Language.PureScript.Options where

import Prelude.Compat

-- | The data type of compiler options
data Options = Options
  { optionsVerboseErrors :: Bool
  -- ^ Verbose error message
  , optionsNoComments :: Bool
  -- ^ Remove the comments from the generated js
  , optionsSourceMaps :: Bool
  -- ^ Generate source maps
  , optionsDumpCoreFn :: Bool
  -- ^ Dump CoreFn
  } deriving Show

-- Default make options
defaultOptions :: Options
defaultOptions = Options False False False False
