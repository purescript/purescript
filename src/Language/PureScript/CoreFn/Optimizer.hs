{-# LANGUAGE FlexibleContexts #-}
module Language.PureScript.CoreFn.Optimizer (optimize) where

import Control.Monad.Reader (asks, MonadReader)
import Control.Monad.Supply.Class (MonadSupply)
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Module (Module)
import Language.PureScript.CoreFn.Optimizer.PassThroughCases
import Language.PureScript.Options (Options, optionsNoOptimizations)

-- |
-- Apply a series of optimizer passes to CoreFn
--
optimize :: (Monad m, MonadReader Options m, MonadSupply m)
         => Module Ann
         -> m (Module Ann)
optimize m = do
  noOpt <- asks optionsNoOptimizations
  if noOpt then return m else optimize' m

optimize' :: (Monad m, MonadSupply m) => Module Ann -> m (Module Ann)
optimize' = go passes
  where passes = [ passThroughCases
                 ]
        go [] m = return m
        go (p : ps) m = p m >>= go ps
