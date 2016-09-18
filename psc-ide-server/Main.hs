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
{-# LANGUAGE NoImplicitPrelude     #-}

module Main where

import           Protolude

import qualified Data.Aeson as Aeson
import           Control.Concurrent.STM
import           "monad-logger" Control.Monad.Logger
import qualified Data.Text.IO                      as T
import qualified Data.ByteString.Lazy.Char8        as BS8
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
import           Options.Applicative               (ParseError (..))
import qualified Options.Applicative               as Opts
import           System.Directory
import           System.FilePath
import           System.IO                         hiding (putStrLn, print)
import           System.IO.Error                   (isEOFError)

import qualified Paths_purescript                  as Paths

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

data Options = Options
  { optionsDirectory  :: Maybe FilePath
  , optionsGlobs      :: [FilePath]
  , optionsOutputPath :: FilePath
  , optionsPort       :: PortNumber
  , optionsNoWatch    :: Bool
  , optionsDebug      :: Bool
  }

main :: IO ()
main = do
  Options dir globs outputPath port noWatch debug <- Opts.execParser opts
  maybe (pure ()) setCurrentDirectory dir
  ideState <- newTVarIO emptyIdeState
  cwd <- getCurrentDirectory
  let fullOutputPath = cwd </> outputPath

  unlessM (doesDirectoryExist fullOutputPath) $ do
    putStrLn ("Your output directory didn't exist. I'll create it at: " <> fullOutputPath)
    createDirectory fullOutputPath
    putText "This usually means you didn't compile your project yet."
    putText "psc-ide needs you to compile your project (for example by running pulp build)"

  unless noWatch $
    void (forkFinally (watcher ideState fullOutputPath) print)

  let conf = Configuration {confDebug = debug, confOutputPath = outputPath, confGlobs = globs}
      env = IdeEnvironment {ideStateVar = ideState, ideConfiguration = conf}
  startServer port env
  where
    parser =
      Options
        <$> optional (Opts.strOption (Opts.long "directory" `mappend` Opts.short 'd'))
        <*> many (Opts.argument Opts.str (Opts.metavar "Source GLOBS..."))
        <*> Opts.strOption (Opts.long "output-directory" `mappend` Opts.value "output/")
        <*> (fromIntegral <$>
             Opts.option Opts.auto (Opts.long "port" `mappend` Opts.short 'p' `mappend` Opts.value (4242 :: Integer)))
        <*> Opts.switch (Opts.long "no-watch")
        <*> Opts.switch (Opts.long "debug")
    opts = Opts.info (version <*> Opts.helper <*> parser) mempty
    version = Opts.abortOption
      (InfoMsg (showVersion Paths.version))
      (Opts.long "version" `mappend` Opts.help "Show the version number")

startServer :: PortNumber -> IdeEnvironment -> IO ()
startServer port env = withSocketsDo $ do
  sock <- listenOnLocalhost port
  runLogger (runReaderT (forever (loop sock)) env)
  where
    runLogger = runStdoutLoggingT . filterLogger (\_ _ -> confDebug (ideConfiguration env))

    loop :: (Ide m, MonadLogger m) => Socket -> m ()
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
                Right r  -> liftIO $ BS8.hPutStrLn h (Aeson.encode r)
                Left err -> liftIO $ BS8.hPutStrLn h (Aeson.encode err)
            Nothing -> do
              $(logDebug) ("Parsing the command failed. Command: " <> cmd)
              liftIO $ do
                T.hPutStrLn h (encodeT (GeneralError "Error parsing Command."))
                hFlush stdout
          liftIO (hClose h)


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
