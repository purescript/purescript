-- | The data type of compiler options
module Language.PureScript.Options where

import Prelude
import Data.Set qualified as S
import Data.Map (Map)
import Data.Map qualified as Map

-- | The data type of compiler options
data Options = Options
  { optionsVerboseErrors :: Bool
  -- ^ Verbose error message
  , optionsNoComments :: Bool
  -- ^ Remove the comments from the generated js
  , optionsCodegenTargets :: S.Set CodegenTarget
  -- ^ Codegen targets (JS, CoreFn, etc.)
  , optionsNoCutoff :: Bool
  -- ^ Disable incremental build cut-off: rebuild every module downstream of a
  -- change, even when the changed modules' public interfaces are unaffected.
  } deriving Show

-- Default make options
defaultOptions :: Options
defaultOptions = Options False False (S.singleton JS) False

data CodegenTarget = JS | JSSourceMap | CoreFn | Docs
  deriving (Eq, Ord, Show)

codegenTargets :: Map String CodegenTarget
codegenTargets = Map.fromList
  [ ("js", JS)
  , ("sourcemaps", JSSourceMap)
  , ("corefn", CoreFn)
  , ("docs", Docs)
  ]
