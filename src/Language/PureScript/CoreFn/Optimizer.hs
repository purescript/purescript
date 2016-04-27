{-# LANGUAGE FlexibleContexts #-}
module Language.PureScript.CoreFn.Optimizer (optimize) where

import Control.Monad.Reader (asks, MonadReader)
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Module (Module)
import Language.PureScript.CoreFn.Optimizer.PassThroughCases
import Language.PureScript.Options (Options, optionsNoOptimizations)

-- |
-- Apply a series of optimizer passes to CoreFn
--
optimize :: (Monad m, MonadReader Options m) => Module Ann -> m (Module Ann)
optimize m = do
  noOpt <- asks optionsNoOptimizations
  if noOpt then return m else return (optimize' m)

optimize' :: Module Ann -> Module Ann
optimize' = foldl1 (.) passes
  where passes = [ passThroughCases
                 ]
