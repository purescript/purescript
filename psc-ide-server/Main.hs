-----------------------------------------------------------------------------
--
-- Module      : Main
-- Description : The server accepting commands for psc-ide
-- Copyright   : Christoph Hegemann 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- The server accepting commands for psc-ide
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Prelude                           ()
import           Prelude.Compat

import           Control.Concurrent                (forkFinally)
import           Control.Concurrent.STM
import           Control.Exception                 (bracketOnError, catchJust)
import           Control.Monad
import           Control.Monad.Error.Class
import           "monad-logger" Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as T
import           Data.Version                      (showVersion)
import           Language.PureScript.Ide
import           Language.PureScript.Ide.Util
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Watcher
import           Network                           hiding (socketPort, accept)
import           Network.BSD                       (getProtocolNumber)
import           Network.Socket                    hiding (PortNumber, Type,
                                                    sClose)
import           Options.Applicative
import           System.Directory
import           System.FilePath
import           System.IO
import           System.IO.Error                   (isEOFError)

import qualified Paths_purescript                  as Paths

-- "Borrowed" from the Idris Compiler
-- Copied from upstream impl of listenOn
-- bound to localhost interface instead of iNADDR_ANY
listenOnLocalhost :: PortID -> IO Socket
listenOnLocalhost (PortNumber port) = do
  proto <- getProtocolNumber "tcp"
  localhost <- inet_addr "127.0.0.1"
  bracketOnError
    (socket AF_INET Stream proto)
    sClose
    (\sock -> do
      setSocketOption sock ReuseAddr 1
      bindSocket sock (SockAddrInet port localhost)
      listen sock maxListenQueue
      pure sock)
listenOnLocalhost _ = error "Wrong Porttype"

data Options = Options
  { optionsDirectory  :: Maybe FilePath
  , optionsOutputPath :: FilePath
  , optionsPort       :: PortID
  , optionsNoWatch    :: Bool
  , optionsDebug      :: Bool
  }

main :: IO ()
main = do
  Options dir outputPath port noWatch debug  <- execParser opts
  maybe (pure ()) setCurrentDirectory dir
  serverState <- newTVarIO emptyPscIdeState
  cwd <- getCurrentDirectory
  let fullOutputPath = cwd </> outputPath

  doesDirectoryExist fullOutputPath
    >>= flip unless
    (do putStrLn ("Your output directory didn't exist. I'll create it at: " <> fullOutputPath)
        createDirectory fullOutputPath
        putStrLn "This usually means you didn't compile your project yet."
        putStrLn "psc-ide needs you to compile your project (for example by running pulp build)")

  unless noWatch $
    void (forkFinally (watcher serverState fullOutputPath) print)

  let conf = Configuration {confDebug = debug, confOutputPath = outputPath}
      env = PscIdeEnvironment {envStateVar = serverState, envConfiguration = conf}
  startServer port env
  where
    parser =
      Options
        <$> optional (strOption (long "directory" <> short 'd'))
        <*> strOption (long "output-directory" <> value "output/")
        <*> (PortNumber . fromIntegral <$>
             option auto (long "port" <> short 'p' <> value (4242 :: Integer)))
        <*> switch (long "no-watch")
        <*> switch (long "debug")
    opts = info (version <*> helper <*> parser) mempty
    version = abortOption
      (InfoMsg (showVersion Paths.version))
      (long "version" <> help "Show the version number")

startServer :: PortID -> PscIdeEnvironment -> IO ()
startServer port env = withSocketsDo $ do
  sock <- listenOnLocalhost port
  runLogger (runReaderT (forever (loop sock)) env)
  where
    runLogger = runStdoutLoggingT . filterLogger (\_ _ -> confDebug (envConfiguration env))

    loop :: (PscIde m, MonadLogger m) => Socket -> m ()
    loop sock = do
      accepted <- runExceptT $ acceptCommand sock
      case accepted of
        Left err -> $(logDebug) err
        Right (cmd, h) -> do
          case decodeT cmd of
            Just cmd' -> do
              result <- runExceptT (handleCommand cmd')
              -- $(logDebug) ("Answer was: " <> T.pack (show result))
              liftIO (hFlush stdout)
              case result of
                -- What function can I use to clean this up?
                Right r  -> liftIO $ T.hPutStrLn h (encodeT r)
                Left err -> liftIO $ T.hPutStrLn h (encodeT err)
            Nothing -> do
              $(logDebug) ("Parsing the command failed. Command: " <> cmd)
              liftIO $ do
                T.hPutStrLn h (encodeT (GeneralError "Error parsing Command."))
                hFlush stdout
          liftIO (hClose h)


acceptCommand :: (MonadIO m, MonadLogger m, MonadError T.Text m)
                 => Socket -> m (T.Text, Handle)
acceptCommand sock = do
  h <- acceptConnection
  $(logDebug) "Accepted a connection"
  cmd' <- liftIO (catchJust
                  -- this means that the connection was
                  -- terminated without receiving any input
                  (\e -> if isEOFError e then Just () else Nothing)
                  (Just <$> T.hGetLine h)
                  (const (pure Nothing)))
  case cmd' of
    Nothing -> throwError "Connection was closed before any input arrived"
    Just cmd -> do
      $(logDebug) cmd
      pure (cmd, h)
  where
   acceptConnection = liftIO $ do
     -- Use low level accept to prevent accidental reverse name resolution
     (s,_) <- accept sock
     h     <- socketToHandle s ReadWriteMode
     hSetEncoding h utf8
     hSetBuffering h LineBuffering
     pure h
