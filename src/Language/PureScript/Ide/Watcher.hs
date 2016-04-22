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

module Language.PureScript.Ide.Watcher where

import           Control.Concurrent              (threadDelay)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Trans.Except
import           Language.PureScript.Ide.Externs
import           Language.PureScript.Ide.State
import           Language.PureScript.Ide.Types
import           Prelude
import           System.FilePath
import           System.FSNotify

-- | Reloads an ExternsFile from Disc. If the Event indicates the ExternsFile
-- was deleted we don't do anything.
reloadFile :: TVar PscIdeState -> Event -> IO ()
reloadFile _ Removed{} = pure ()
reloadFile stateVar ev = do
  let fp = eventPath ev
  ef' <- runExceptT (readExternFile fp)
  case ef' of
    Left _ -> pure ()
    Right ef -> do
      atomically (insertModuleSTM stateVar ef)
      putStrLn ("Reloaded File at: " ++ fp)

-- | Installs filewatchers for the given directory and reloads ExternsFiles when
-- they change on disc
watcher :: TVar PscIdeState -> FilePath -> IO ()
watcher stateVar fp =
  withManagerConf (defaultConfig { confDebounce = NoDebounce }) $ \mgr -> do
    _ <- watchTree mgr fp
      (\ev -> takeFileName (eventPath ev) == "externs.json")
      (reloadFile stateVar)
    forever (threadDelay 100000)
