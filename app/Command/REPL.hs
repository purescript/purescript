{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Command.REPL (command) where

import           Prelude ()
import           Prelude.Compat
import           Control.Applicative (many, (<|>))
import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar,
                                         tryPutMVar)
import           Control.Concurrent.STM (TVar, atomically, newTVarIO, writeTVar,
                                        readTVarIO,
                                        TChan, newBroadcastTChanIO, dupTChan,
                                        readTChan, writeTChan)
import           Control.Exception (fromException)
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.State.Strict (StateT, evalStateT)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Data.FileEmbed (embedStringFile)
import           Data.Monoid ((<>))
import           Data.String (IsString(..))
import           Data.Text (Text, unpack)
import           Data.Traversable (for)
import qualified Language.PureScript as P
import qualified Language.PureScript.Bundle as Bundle
import           Language.PureScript.Interactive
import           Network.HTTP.Types.Header (hContentType, hCacheControl,
                                            hPragma, hExpires)
import           Network.HTTP.Types.Status (status200, status404, status503)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS
import qualified Options.Applicative as Opts
import           System.Console.Haskeline
import           System.IO.UTF8 (readUTF8File)
import           System.Exit
import           System.FilePath ((</>))
import           System.FilePath.Glob (glob)
import           System.Process (readProcessWithExitCode)

-- | Command line options
data PSCiOptions = PSCiOptions
  { psciInputFile         :: [FilePath]
  , psciBackend           :: Backend
  }

inputFile :: Opts.Parser FilePath
inputFile = Opts.strArgument $
     Opts.metavar "FILE"
  <> Opts.help "Optional .purs files to load on start"

nodePathOption :: Opts.Parser (Maybe FilePath)
nodePathOption = Opts.optional . Opts.strOption $
     Opts.metavar "FILE"
  <> Opts.long "node-path"
  <> Opts.help "Path to the Node executable"

nodeFlagsOption :: Opts.Parser [String]
nodeFlagsOption = Opts.option parser $
     Opts.long "node-opts"
  <> Opts.metavar "OPTS"
  <> Opts.value []
  <> Opts.help "Flags to pass to node, separated by spaces"
  where
    parser = words <$> Opts.str

port :: Opts.Parser Int
port = Opts.option Opts.auto $
     Opts.long "port"
  <> Opts.short 'p'
  <> Opts.help "The web server port"

backend :: Opts.Parser Backend
backend =
  (browserBackend <$> port)
  <|> (nodeBackend <$> nodePathOption <*> nodeFlagsOption)

psciOptions :: Opts.Parser PSCiOptions
psciOptions = PSCiOptions <$> many inputFile
                          <*> backend

-- | Parses the input and returns either a command, or an error as a 'String'.
getCommand :: forall m. MonadException m => InputT m (Either String (Maybe Command))
getCommand = handleInterrupt (return (Right Nothing)) $ do
  line <- withInterrupt $ getInputLine "> "
  case line of
    Nothing -> return (Right (Just QuitPSCi)) -- Ctrl-D when input is empty
    Just "" -> return (Right Nothing)
    Just s  -> return . fmap Just $ parseCommand s

pasteMode :: forall m. MonadException m => InputT m (Either String Command)
pasteMode =
    parseCommand <$> go []
  where
    go :: [String] -> InputT m String
    go ls = maybe (return . unlines $ reverse ls) (go . (:ls)) =<< getInputLine "… "

-- | Make a JavaScript bundle for the browser.
bundle :: IO (Either Bundle.ErrorMessage String)
bundle = runExceptT $ do
  inputFiles <- liftIO (glob (".psci_modules" </> "node_modules" </> "*" </> "*.js"))
  input <- for inputFiles $ \filename -> do
    js <- liftIO (readUTF8File filename)
    mid <- Bundle.guessModuleIdentifier filename
    length js `seq` return (mid, js)
  Bundle.bundle input [] Nothing "PSCI"

indexJS :: IsString string => string
indexJS = $(embedStringFile "app/static/index.js")

indexPage :: IsString string => string
indexPage = $(embedStringFile "app/static/index.html")

-- | All of the functions required to implement a PSCi backend
data Backend = forall state. Backend
  { _backendSetup :: IO state
  -- ^ Initialize, and call the continuation when the backend is ready
  , _backendEval :: state -> String -> IO ()
  -- ^ Evaluate JavaScript code
  , _backendReload :: state -> IO ()
  -- ^ Reload the compiled code
  , _backendShutdown :: state -> IO ()
  -- ^ Shut down the backend
  }

-- | Commands which can be sent to the browser
data BrowserCommand
  = Eval (MVar String)
  -- ^ Evaluate the latest JS
  | Reload
  -- ^ Reload the page

-- | State for the browser backend
data BrowserState = BrowserState
  { browserCommands       :: TChan BrowserCommand
  -- ^ A channel which receives data when the compiled JS has
  -- been updated
  , browserShutdownNotice :: MVar ()
  -- ^ An MVar which becomes full when the server should shut down
  , browserIndexJS        :: TVar (Maybe String)
  -- ^ A TVar holding the latest compiled JS
  , browserBundleJS       :: TVar (Maybe String)
  -- ^ A TVar holding the latest bundled JS
  }

browserBackend :: Int -> Backend
browserBackend serverPort = Backend setup evaluate reload shutdown
  where
    setup :: IO BrowserState
    setup = do
      shutdownVar <- newEmptyMVar
      cmdChan <- newBroadcastTChanIO
      indexJs <- newTVarIO Nothing
      bundleJs <- newTVarIO Nothing

      let
        handleWebsocket :: WS.PendingConnection -> IO ()
        handleWebsocket pending = do
          conn <- WS.acceptRequest pending
          -- Fork a thread to keep the connection alive
          WS.forkPingThread conn 10
          -- Clone the command channel
          cmdChanCopy <- atomically $ dupTChan cmdChan
          -- Listen for commands
          forever $ do
            cmd <- atomically $ readTChan cmdChanCopy
            case cmd of
              Eval resultVar -> void $ do
                WS.sendTextData conn ("eval" :: Text)
                result <- WS.receiveData conn
                -- With many connected clients, all but one of
                -- these attempts will fail.
                tryPutMVar resultVar (unpack result)
              Reload ->
                WS.sendTextData conn ("reload" :: Text)

        shutdownHandler :: IO () -> IO ()
        shutdownHandler stopServer = void . forkIO $ do
          () <- takeMVar shutdownVar
          stopServer

        onException :: Maybe Wai.Request -> SomeException -> IO ()
        onException req ex
          | Just (_ :: WS.ConnectionException) <- fromException ex
          = return () -- ignore websocket disconnects
          | otherwise = Warp.defaultOnException req ex

        staticServer :: Wai.Application
        staticServer req respond =
          case Wai.pathInfo req of
            [] ->
              respond $ Wai.responseLBS status200
                                        [(hContentType, "text/html")]
                                        indexPage
            ["js", "index.js"] ->
              respond $ Wai.responseLBS status200
                                        [(hContentType, "application/javascript")]
                                        indexJS
            ["js", "latest.js"] -> do
              may <- readTVarIO indexJs
              case may of
                Nothing ->
                  respond $ Wai.responseLBS status503 [] "Service not available"
                Just js ->
                  respond $ Wai.responseLBS status200
                                            [ (hContentType, "application/javascript")
                                            , (hCacheControl, "no-cache, no-store, must-revalidate")
                                            , (hPragma, "no-cache")
                                            , (hExpires, "0")
                                            ]
                                            (fromString js)
            ["js", "bundle.js"] -> do
              may <- readTVarIO bundleJs
              case may of
                Nothing ->
                  respond $ Wai.responseLBS status503 [] "Service not available"
                Just js ->
                  respond $ Wai.responseLBS status200
                                            [ (hContentType, "application/javascript")]
                                            (fromString js)
            _ -> respond $ Wai.responseLBS status404 [] "Not found"

      let browserState = BrowserState cmdChan shutdownVar indexJs bundleJs
      createBundle browserState

      putStrLn $ "Serving http://localhost:" <> show serverPort <> "/. Waiting for connections..."
      _ <- forkIO $ Warp.runSettings ( Warp.setInstallShutdownHandler shutdownHandler
                                     . Warp.setPort serverPort
                                     . Warp.setOnException onException
                                     $ Warp.defaultSettings
                                     ) $
                      WS.websocketsOr WS.defaultConnectionOptions
                                      handleWebsocket
                                      staticServer
      return browserState

    createBundle :: BrowserState -> IO ()
    createBundle state = do
      putStrLn "Bundling JavaScript..."
      ejs <- bundle
      case ejs of
        Left err -> do
          putStrLn (unlines (Bundle.printErrorMessage err))
          exitFailure
        Right js ->
          atomically $ writeTVar (browserBundleJS state) (Just js)

    reload :: BrowserState -> IO ()
    reload state = do
      createBundle state
      atomically $ writeTChan (browserCommands state) Reload

    shutdown :: BrowserState -> IO ()
    shutdown state = putMVar (browserShutdownNotice state) ()

    evaluate :: BrowserState -> String -> IO ()
    evaluate state js = liftIO $ do
      resultVar <- newEmptyMVar
      atomically $ do
        writeTVar (browserIndexJS state) (Just js)
        writeTChan (browserCommands state) (Eval resultVar)
      result <- takeMVar resultVar
      putStrLn result

nodeBackend :: Maybe FilePath -> [String] -> Backend
nodeBackend nodePath nodeArgs = Backend setup eval reload shutdown
  where
    setup :: IO ()
    setup = return ()

    eval :: () -> String -> IO ()
    eval _ _ = do
      writeFile indexFile "require('$PSCI')['$main']();"
      process <- maybe findNodeProcess (pure . pure) nodePath
      result <- traverse (\node -> readProcessWithExitCode node (nodeArgs ++ [indexFile]) "") process
      case result of
        Just (ExitSuccess, out, _)   -> putStrLn out
        Just (ExitFailure _, _, err) -> putStrLn err
        Nothing                      -> putStrLn "Couldn't find node.js"

    reload :: () -> IO ()
    reload _ = return ()

    shutdown :: () -> IO ()
    shutdown _ = return ()

options :: Opts.Parser PSCiOptions
options = Opts.helper <*> psciOptions

-- | Get command line options and drop into the REPL
command :: Opts.Parser (IO ())
command = loop <$> options
  where
    loop :: PSCiOptions -> IO ()
    loop PSCiOptions{..} = do
        inputFiles <- concat <$> traverse glob psciInputFile
        e <- runExceptT $ do
          modules <- ExceptT (loadAllModules inputFiles)
          when (null modules) . liftIO $ do
            putStr noInputMessage
            exitFailure
          unless (supportModuleIsDefined (map snd modules)) . liftIO $ do
            putStr supportModuleMessage
            exitFailure
          (externs, env) <- ExceptT . runMake . make $ modules
          return (modules, externs, env)
        case psciBackend of
          Backend setup eval reload (shutdown :: state -> IO ()) ->
            case e of
              Left errs -> putStrLn (P.prettyPrintMultipleErrors P.defaultPPEOptions errs) >> exitFailure
              Right (modules, externs, env) -> do
                historyFilename <- getHistoryFilename
                let settings = defaultSettings { historyFile = Just historyFilename }
                    initialState = PSCiState [] [] (zip (map snd modules) externs)
                    config = PSCiConfig inputFiles env
                    runner = flip runReaderT config
                             . flip evalStateT initialState
                             . runInputT (setComplete completion settings)

                    go :: state -> InputT (StateT PSCiState (ReaderT PSCiConfig IO)) ()
                    go state = do
                      c <- getCommand
                      case c of
                        Left err -> outputStrLn err >> go state
                        Right Nothing -> go state
                        Right (Just PasteLines) -> do
                          c' <- pasteMode
                          case c' of
                            Left err -> outputStrLn err >> go state
                            Right c'' -> handleCommandWithInterrupts state c''
                        Right (Just QuitPSCi) -> do
                          outputStrLn quitMessage
                          liftIO $ shutdown state
                        Right (Just c') -> handleCommandWithInterrupts state c'

                    handleCommandWithInterrupts
                      :: state
                      -> Command
                      -> InputT (StateT PSCiState (ReaderT PSCiConfig IO)) ()
                    handleCommandWithInterrupts state cmd = do
                      handleInterrupt (outputStrLn "Interrupted.")
                                      (withInterrupt (lift (handleCommand (liftIO . eval state) (liftIO (reload state)) cmd)))
                      go state

                putStrLn prologueMessage
                setup >>= runner . go
