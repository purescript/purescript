{-# LANGUAGE PackageImports #-}
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

module Language.PureScript.Ide.Watcher
 ( watcher
 ) where

import                Protolude

import                Control.Concurrent.STM
import "monad-logger" Control.Monad.Logger
import                Language.PureScript.Ide.Externs
import                Language.PureScript.Ide.State
import                Language.PureScript.Ide.Types
import                Language.PureScript.Ide.Util
import                System.FSNotify
import                System.FilePath

-- | Reloads an ExternsFile from Disc. If the Event indicates the ExternsFile
-- was deleted we don't do anything.
reloadFile :: IdeLogLevel -> TVar IdeState -> Event -> IO ()
reloadFile _ _ Removed{} = pure ()
reloadFile logLevel ref ev = runLogger logLevel $ do
  let fp = eventPath ev
  ef' <- runExceptT (readExternFile fp)
  case ef' of
    Left err ->
      logErrorN ("Failed to reload file at: " <> toS fp <> " with error: " <> show err)
    Right ef -> do
      lift $ void $ atomically (insertExternsSTM ref ef *> populateVolatileStateSTM ref)
      logDebugN ("Reloaded File at: " <> toS fp)

-- | Installs filewatchers for the given directory and reloads ExternsFiles when
-- they change on disc
watcher :: Bool -> IdeLogLevel -> TVar IdeState -> FilePath -> IO ()
watcher polling logLevel stateVar fp =
  withManagerConf
    (defaultConfig { confDebounce = NoDebounce
                   , confUsePolling = polling
                   }) $ \mgr -> do
      _ <- watchTree mgr fp
        (\ev -> takeFileName (eventPath ev) == "externs.json")
        (reloadFile logLevel stateVar)
      forever (threadDelay 100000)
