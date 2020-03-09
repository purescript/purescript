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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Command.Ide (command) where

import           Protolude

import qualified Data.Aeson as Aeson
import           Control.Concurrent.STM
import           "monad-logger" Control.Monad.Logger
import           Data.IORef
import qualified Data.Text.IO                      as T
import qualified Data.ByteString.Char8             as BS8
import qualified Data.ByteString.Lazy.Char8        as BSL8
import           GHC.IO.Exception                  (IOErrorType(..), IOException(..))
import           Language.PureScript.Ide
import           Language.PureScript.Ide.Command
import           Language.PureScript.Ide.Util
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.State (updateCacheTimestamp)
import           Language.PureScript.Ide.Types
import qualified Network.Socket                    as Network
import qualified Options.Applicative               as Opts
import           System.Directory
import           System.FilePath
import           System.IO                         hiding (putStrLn, print)
import           System.IO.Error                   (isEOFError)

listenOnLocalhost :: Network.PortNumber -> IO Network.Socket
listenOnLocalhost port = do
  let hints = Network.defaultHints
        { Network.addrFamily = Network.AF_INET
        , Network.addrSocketType = Network.Stream
        }
  addr:_ <- Network.getAddrInfo (Just hints) (Just "127.0.0.1") (Just (show port))
  bracketOnError
    (Network.socket (Network.addrFamily addr) (Network.addrSocketType addr) (Network.addrProtocol addr))
    Network.close
    (\sock -> do
      Network.setSocketOption sock Network.ReuseAddr 1
      Network.bind sock (Network.addrAddress addr)
      Network.listen sock Network.maxListenQueue
      pure sock)

data ServerOptions = ServerOptions
  { _serverDirectory  :: Maybe FilePath
  , _serverGlobs      :: [FilePath]
  , _serverOutputPath :: FilePath
  , _serverPort       :: Network.PortNumber
  , _serverLoglevel   :: IdeLogLevel
  -- TODO(Christoph) Deprecated
  , _serverEditorMode :: Bool
  , _serverPolling    :: Bool
  , _serverNoWatch    :: Bool
  } deriving (Show)

data ClientOptions = ClientOptions
  { clientPort :: Network.PortNumber
  }

command :: Opts.Parser (IO ())
command = Opts.helper <*> subcommands where
  subcommands :: Opts.Parser (IO ())
  subcommands = (Opts.subparser . fold)
    [ Opts.command "server"
        (Opts.info (fmap server serverOptions <**> Opts.helper)
          (Opts.progDesc "Start a server process"))
    , Opts.command "client"
        (Opts.info (fmap client clientOptions <**> Opts.helper)
          (Opts.progDesc "Connect to a running server"))
    ]

  client :: ClientOptions -> IO ()
  client ClientOptions{..} = do
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    let handler (SomeException e) = do
          T.putStrLn ("Couldn't connect to purs ide server on port " <> show clientPort <> ":")
          print e
          exitFailure
    let hints = Network.defaultHints
          { Network.addrFamily = Network.AF_INET
          , Network.addrSocketType = Network.Stream
          }
    addr:_ <- Network.getAddrInfo (Just hints) (Just "127.0.0.1") (Just (show clientPort))
    sock <- Network.socket (Network.addrFamily addr) (Network.addrSocketType addr) (Network.addrProtocol addr)
    Network.connect sock (Network.addrAddress addr) `catch` handler
    h <- Network.socketToHandle sock ReadWriteMode
    T.hPutStrLn h =<< T.getLine
    BS8.putStrLn =<< BS8.hGetLine h
    hFlush stdout
    hClose h

  clientOptions :: Opts.Parser ClientOptions
  clientOptions = ClientOptions . fromIntegral <$>
    Opts.option Opts.auto (Opts.long "port" `mappend` Opts.short 'p' `mappend` Opts.value (4242 :: Integer))

  server :: ServerOptions -> IO ()
  server opts'@(ServerOptions dir globs outputPath port logLevel editorMode polling noWatch) = do
    when (logLevel == LogDebug || logLevel == LogAll)
      (putText "Parsed Options:" *> print opts')
    maybe (pure ()) setCurrentDirectory dir
    ideState <- newTVarIO emptyIdeState
    cwd <- getCurrentDirectory
    let fullOutputPath = cwd </> outputPath

    when editorMode
      (putText "The --editor-mode flag is deprecated and ignored. It's now the default behaviour and the flag will be removed in a future version")

    when polling
      (putText "The --polling flag is deprecated and ignored. purs ide no longer uses a file system watcher, instead it relies on its clients to notify it about updates and checks timestamps to invalidate itself")

    when noWatch
      (putText "The --no-watch flag is deprecated and ignored. purs ide no longer uses a file system watcher, instead it relies on its clients to notify it about updates and checks timestamps to invalidate itself")

    unlessM (doesDirectoryExist fullOutputPath) $ do
      putText "Your output directory didn't exist. This usually means you didn't compile your project yet."
      putText "psc-ide needs you to compile your project (for example by running pulp build)"

    let
      conf = IdeConfiguration
        { confLogLevel = logLevel
        , confOutputPath = outputPath
        , confGlobs = globs
        }
    ts <- newIORef Nothing
    let
      env = IdeEnvironment
        { ideStateVar = ideState
        , ideConfiguration = conf
        , ideCacheDbTimestamp = ts
        }
    startServer port env

  serverOptions :: Opts.Parser ServerOptions
  serverOptions =
    ServerOptions
      <$> optional (Opts.strOption (Opts.long "directory" `mappend` Opts.short 'd'))
      <*> many (Opts.argument Opts.str (Opts.metavar "Source GLOBS..."))
      <*> Opts.strOption (Opts.long "output-directory" `mappend` Opts.value "output/")
      <*> (fromIntegral <$>
           Opts.option Opts.auto (Opts.long "port" `mappend` Opts.short 'p' `mappend` Opts.value (4242 :: Integer)))
      <*> (parseLogLevel <$> Opts.strOption
           (Opts.long "log-level"
            `mappend` Opts.value ""
            `mappend` Opts.help "One of \"debug\", \"perf\", \"all\" or \"none\""))
      -- TODO(Christoph): Deprecated
      <*> Opts.switch (Opts.long "editor-mode")
      <*> Opts.switch (Opts.long "no-watch")
      <*> Opts.switch (Opts.long "polling")

  parseLogLevel :: Text -> IdeLogLevel
  parseLogLevel s = case s of
    "debug" -> LogDebug
    "perf" -> LogPerf
    "all" -> LogAll
    "none" -> LogNone
    _ -> LogDefault

startServer :: Network.PortNumber -> IdeEnvironment -> IO ()
startServer port env = Network.withSocketsDo $ do
  sock <- listenOnLocalhost port
  runLogger (confLogLevel (ideConfiguration env)) (runReaderT (forever (loop sock)) env)
  where
    loop :: (Ide m, MonadLogger m) => Network.Socket -> m ()
    loop sock = do
      accepted <- runExceptT (acceptCommand sock)
      case accepted of
        Left err -> $(logError) err
        Right (cmd, h) -> do
          case decodeT cmd of
            Just cmd' -> do
              let message duration =
                    "Command "
                      <> commandName cmd'
                      <> " took "
                      <> displayTimeSpec duration
              logPerf message $ do
                result <- runExceptT $ do
                  updateCacheTimestamp >>= \case
                    Nothing -> pure ()
                    Just (before, after) -> do
                      -- If the cache db file was changed outside of the IDE
                      -- we trigger a reset before processing the command
                      $(logInfo) ("cachedb was changed from: " <> show before <> ", to: " <> show after)
                      unless (isLoadAll cmd') $
                        void (handleCommand Reset *> handleCommand (LoadSync []))
                  handleCommand cmd'
                liftIO $ catchGoneHandle $ BSL8.hPutStrLn h $ case result of
                  Right r  -> Aeson.encode r
                  Left err -> Aeson.encode err
              liftIO (hFlush stdout)
            Nothing -> do
              $(logError) ("Parsing the command failed. Command: " <> cmd)
              liftIO $ do
                catchGoneHandle (T.hPutStrLn h (encodeT (GeneralError "Error parsing Command.")))
                hFlush stdout
          liftIO $ catchGoneHandle (hClose h)

isLoadAll :: Command -> Bool
isLoadAll = \case
  Load [] -> True
  _ -> False

catchGoneHandle :: IO () -> IO ()
catchGoneHandle =
  handle (\e -> case e of
    IOError { ioe_type = ResourceVanished } ->
      putText ("[Error] psc-ide-server tried to interact with the handle, but the connection was already gone.")
    _ -> throwIO e)

acceptCommand
  :: (MonadIO m, MonadLogger m, MonadError Text m)
  => Network.Socket
  -> m (Text, Handle)
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
      $(logDebug) ("Received command: " <> cmd)
      pure (cmd, h)
  where
   acceptConnection = liftIO $ do
     -- Use low level accept to prevent accidental reverse name resolution
     (s,_) <- Network.accept sock
     h     <- Network.socketToHandle s ReadWriteMode
     hSetEncoding h utf8
     hSetBuffering h LineBuffering
     pure h
