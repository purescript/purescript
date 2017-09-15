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
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Command.Ide (command) where

import           Protolude

import qualified Data.Aeson as Aeson
import           Control.Concurrent.STM
import           "monad-logger" Control.Monad.Logger
import qualified Data.Text.IO                      as T
import qualified Data.ByteString.Char8             as BS8
import qualified Data.ByteString.Lazy.Char8        as BSL8
import           GHC.IO.Exception                  (IOErrorType(..), IOException(..))
import           Language.PureScript.Ide
import           Language.PureScript.Ide.Command
import           Language.PureScript.Ide.Util
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Watcher
import           Network                           hiding (socketPort, accept)
import           Network.BSD                       (getProtocolNumber)
import           Network.Socket                    hiding (PortNumber, Type,
                                                    sClose)
import qualified Options.Applicative               as Opts
import           System.Directory
import           System.Info                       as SysInfo
import           System.FilePath
import           System.IO                         hiding (putStrLn, print)
import           System.IO.Error                   (isEOFError)

listenOnLocalhost :: PortNumber -> IO Socket
listenOnLocalhost port = do
  proto <- getProtocolNumber "tcp"
  localhost <- inet_addr "127.0.0.1"
  bracketOnError
    (socket AF_INET Stream proto)
    sClose
    (\sock -> do
      setSocketOption sock ReuseAddr 1
      bind sock (SockAddrInet port localhost)
      listen sock maxListenQueue
      pure sock)

data ServerOptions = ServerOptions
  { _serverDirectory  :: Maybe FilePath
  , _serverGlobs      :: [FilePath]
  , _serverOutputPath :: FilePath
  , _serverPort       :: PortNumber
  , _serverNoWatch    :: Bool
  , _serverPolling    :: Bool
  , _serverLoglevel   :: IdeLogLevel
  , _serverEditorMode :: Bool
  } deriving (Show)

data ClientOptions = ClientOptions
  { clientPort :: PortID
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
    h <- connectTo "127.0.0.1" clientPort `catch` handler
    T.hPutStrLn h =<< T.getLine
    BS8.putStrLn =<< BS8.hGetLine h
    hFlush stdout
    hClose h

  clientOptions :: Opts.Parser ClientOptions
  clientOptions = ClientOptions . PortNumber . fromIntegral <$>
    Opts.option Opts.auto (Opts.long "port" <> Opts.short 'p' <> Opts.value (4242 :: Integer))

  server :: ServerOptions -> IO ()
  server opts'@(ServerOptions dir globs outputPath port noWatch polling logLevel editorMode) = do
    when (logLevel == LogDebug || logLevel == LogAll)
      (putText "Parsed Options:" *> print opts')
    maybe (pure ()) setCurrentDirectory dir
    ideState <- newTVarIO emptyIdeState
    cwd <- getCurrentDirectory
    let fullOutputPath = cwd </> outputPath

    unlessM (doesDirectoryExist fullOutputPath) $ do
      putText "Your output directory didn't exist. This usually means you didn't compile your project yet."
      putText "psc-ide needs you to compile your project (for example by running pulp build)"

    unless (noWatch || editorMode) $
      void (forkFinally (watcher polling logLevel ideState fullOutputPath) print)
    let
      conf = IdeConfiguration
        { confLogLevel = logLevel
        , confOutputPath = outputPath
        , confGlobs = globs
        , confEditorMode = editorMode
        }
    let env = IdeEnvironment {ideStateVar = ideState, ideConfiguration = conf}
    startServer port env

  serverOptions :: Opts.Parser ServerOptions
  serverOptions =
    ServerOptions
      <$> optional (Opts.strOption (Opts.long "directory" `mappend` Opts.short 'd'))
      <*> many (Opts.argument Opts.str (Opts.metavar "Source GLOBS..."))
      <*> Opts.strOption (Opts.long "output-directory" `mappend` Opts.value "output/")
      <*> (fromIntegral <$>
           Opts.option Opts.auto (Opts.long "port" `mappend` Opts.short 'p' `mappend` Opts.value (4242 :: Integer)))
      <*> Opts.switch (Opts.long "no-watch")
      <*> flipIfWindows (Opts.switch (Opts.long "polling"))
      <*> (parseLogLevel <$> Opts.strOption
           (Opts.long "log-level"
            `mappend` Opts.value ""
            `mappend` Opts.help "One of \"debug\", \"perf\", \"all\" or \"none\""))
      <*> Opts.switch (Opts.long "editor-mode")

  parseLogLevel :: Text -> IdeLogLevel
  parseLogLevel s = case s of
    "debug" -> LogDebug
    "perf" -> LogPerf
    "all" -> LogAll
    "none" -> LogNone
    _ -> LogDefault

  -- polling is the default on Windows and the flag turns it off. See
  -- #2209 and #2414 for explanations
  flipIfWindows = map (if SysInfo.os == "mingw32" then not else identity)

startServer :: PortNumber -> IdeEnvironment -> IO ()
startServer port env = withSocketsDo $ do
  sock <- listenOnLocalhost port
  runLogger (confLogLevel (ideConfiguration env)) (runReaderT (forever (loop sock)) env)
  where
    loop :: (Ide m, MonadLogger m) => Socket -> m ()
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
                result <- runExceptT (handleCommand cmd')
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

catchGoneHandle :: IO () -> IO ()
catchGoneHandle =
  handle (\e -> case e of
    IOError { ioe_type = ResourceVanished } ->
      putText ("[Error] psc-ide-server tried interact with the handle, but the connection was already gone.")
    _ -> throwIO e)

acceptCommand :: (MonadIO m, MonadLogger m, MonadError Text m)
                 => Socket -> m (Text, Handle)
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
     (s,_) <- accept sock
     h     <- socketToHandle s ReadWriteMode
     hSetEncoding h utf8
     hSetBuffering h LineBuffering
     pure h
