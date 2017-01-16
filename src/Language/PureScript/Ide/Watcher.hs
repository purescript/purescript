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

import           Protolude

import           Control.Concurrent.STM
import           Language.PureScript.Ide.Externs
import           Language.PureScript.Ide.State
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util
import           System.FilePath
import           System.FSNotify

-- | Reloads an ExternsFile from Disc. If the Event indicates the ExternsFile
-- was deleted we don't do anything.
reloadFile :: TVar IdeState -> Event -> IO ()
reloadFile _ Removed{} = pure ()
reloadFile ref ev = do
  let fp = eventPath ev
  ef' <- runLogger LogDefault (runExceptT (readExternFile fp))
  case ef' of
    Left _ -> pure ()
    Right ef -> do
      void $ atomically (insertExternsSTM ref ef *> populateStage3STM ref)
      putStrLn ("Reloaded File at: " ++ fp)

-- | Installs filewatchers for the given directory and reloads ExternsFiles when
-- they change on disc
watcher :: Bool -> TVar IdeState -> FilePath -> IO ()
watcher polling stateVar fp =
  withManagerConf
    (defaultConfig { confDebounce = NoDebounce
                   , confUsePolling = polling
                   }) $ \mgr -> do
      _ <- watchTree mgr fp
        (\ev -> takeFileName (eventPath ev) == "externs.json")
        (reloadFile stateVar)
      forever (threadDelay 100000)
