-----------------------------------------------------------------------------
--
-- Module      : Language.PureScript.Ide.Watcher
-- Description : File watcher for externs files
-- Copyright   : Christoph Hegemann 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- File watcher for externs files
-----------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

module Language.PureScript.Ide.Watcher where

import           Prelude                         ()
import           Prelude.Compat

import           Control.Concurrent              (threadDelay)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Trans.Except
import qualified Data.Map                        as M
import           Data.Maybe                      (isJust)
import           Language.PureScript.Externs
import           Language.PureScript.Ide.Externs
import           Language.PureScript.Ide.State
import           Language.PureScript.Ide.Types
import           System.FilePath
import           System.FSNotify


reloadFile :: TVar PscIdeState -> FilePath -> IO ()
reloadFile stateVar fp = do
  (Right ef@ExternsFile{..}) <- runExceptT $ readExternFile fp
  reloaded <- atomically $ do
    st <- readTVar stateVar
    if isLoaded efModuleName st
      then
        insertModule' stateVar ef *> pure True
      else
        pure False
  when reloaded $ putStrLn $ "Reloaded File at: " ++ fp
  where
    isLoaded name st = isJust (M.lookup name (externsFiles st))

watcher :: TVar PscIdeState -> FilePath -> IO ()
watcher stateVar fp = withManager $ \mgr -> do
  _ <- watchTree mgr fp
    (\ev -> takeFileName (eventPath ev) == "externs.json")
    (reloadFile stateVar . eventPath)
  forever (threadDelay 10000)
