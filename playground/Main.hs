module Main where

import Protolude

import Data.IORef (IORef, newIORef, writeIORef)
import Language.PureScript.Externs ( ExternsFile )
import Data.String (String)
import Data.Map qualified as M
import Language.PureScript.Make.Monad (readExternsFile)
import System.FilePath.Glob qualified as Glob
import Data.List qualified as L
import System.Console.Haskeline (InputT, getInputLine, runInputT, defaultSettings)

data ReplState = ReplState
  { externsFiles :: IORef (Map FilePath (Maybe ExternsFile))
  }

mkReplState :: IO ReplState
mkReplState = ReplState <$> newIORef mempty

loadExterns :: (MonadIO m, MonadReader ReplState m) => [FilePath] -> m ()
loadExterns files = do
  liftIO $ putStrLn ("Loading externs..." :: String)
  externs <- asks externsFiles
  runExceptT (traverse readExternsFile files) >>= \case
    Left _ ->
      pure ()
    Right !externFiles ->
      liftIO $ writeIORef externs $ M.fromList $ L.zip files externFiles

removeExterns :: (MonadIO m, MonadReader ReplState m) => m ()
removeExterns = do
  liftIO $ putStrLn ("Removing externs..." :: String)
  externsFiles' <- asks externsFiles
  liftIO $ writeIORef externsFiles' mempty

loop :: [FilePath] -> InputT (ReaderT ReplState IO) ()
loop files = do
  i <- getInputLine "> "
  case i of
    Just "load" -> do
      lift $ loadExterns files
      loop files
    Just "unload" -> do
      lift removeExterns
      loop files
    Just "reload" -> do
      lift removeExterns
      lift $ loadExterns files
      loop files
    Just "delay" -> do
      liftIO $ threadDelay 10000000
      loop files
    Just "exit" ->
      pure ()
    _ -> do
      loop files

data ReplState' = ReplState'
  { externsFiles' :: Map FilePath (Maybe ExternsFile)
  }

defaultReplState' :: ReplState'
defaultReplState' = ReplState' mempty

loadExterns' :: (MonadIO m, MonadState ReplState' m) => [FilePath] -> m ()
loadExterns' files = do
  liftIO $ putStrLn ("Loading externs..." :: String)
  runExceptT (traverse readExternsFile files) >>= \case
    Left _ ->
      pure ()
    Right !externFiles ->
      modify (\s -> s { externsFiles' = M.fromList $ L.zip files externFiles })

removeExterns' :: (MonadIO m, MonadState ReplState' m) => m ()
removeExterns' = do
  liftIO $ putStrLn ("Removing externs..." :: String)
  modify (\s -> s { externsFiles' = M.empty })

loop' :: [FilePath] -> InputT (StateT ReplState' IO) ()
loop' files = do
  i <- getInputLine "> "
  case i of
    Just "load" -> do
      lift $ loadExterns' files
      loop' files
    Just "unload" -> do
      lift removeExterns'
      loop' files
    Just "reload" -> do
      lift removeExterns'
      lift $ loadExterns' files
      loop' files
    Just "delay" -> do
      liftIO $ threadDelay 10000000
      loop' files
    Just "exit" ->
      pure ()
    _ -> do
      loop' files


main :: IO ()
main = do
  files <- Glob.glob "tmp/output/**/externs.cbor"

  putStrLn ("Reader: " :: String)
  _ <- do
    replState <- mkReplState
    flip runReaderT replState $ runInputT defaultSettings (loop files)

  putStrLn ("State: " :: String)
  _ <- do
    flip evalStateT defaultReplState' $ runInputT defaultSettings (loop' files)

  pure ()
