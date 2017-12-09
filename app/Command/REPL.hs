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

import           PSPrelude hiding (evalStateT, StateT, onException, state, evaluate)

import           Control.Concurrent.STM (TVar, atomically, newTVarIO, writeTVar,
                                        readTVarIO,
                                        TChan, newBroadcastTChanIO, dupTChan,
                                        readTChan, writeTChan)
import           Control.Exception (fromException)
import           Control.Monad.Trans.State.Strict (StateT, evalStateT)
import           Data.FileEmbed (embedStringFile)
import qualified Data.Text as T
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
import           System.IO.UTF8 (withUTF8FileContentsT, runWithUTF8FileContentsT, writeUTF8FileT)
import           System.Directory (doesFileExist, getCurrentDirectory)
import           System.FilePath ((</>))
import           System.FilePath.Glob (glob)
import           System.Process (readProcessWithExitCode)

-- | Command line options
data PSCiOptions = PSCiOptions
  { psciInputGlob         :: [FilePath]
  , psciBackend           :: Backend
  }

inputFile :: Opts.Parser FilePath
inputFile = Opts.strArgument $
     Opts.metavar "FILES"
  <> Opts.help "Optional .purs files to load on start"

nodePathOption :: Opts.Parser (Maybe FilePath)
nodePathOption = Opts.optional . Opts.strOption $
     Opts.metavar "FILE"
  <> Opts.long "node-path"
  <> Opts.help "Path to the Node executable"

nodeFlagsOption :: Opts.Parser [Text]
nodeFlagsOption = Opts.option parser $
     Opts.long "node-opts"
  <> Opts.metavar "OPTS"
  <> Opts.value []
  <> Opts.help "Flags to pass to node, separated by spaces"
  where
    parser = T.words <$> Opts.str

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

-- | Parses the input and returns either a command, or an error as a 'Text'.
getCommand :: forall m. MonadException m => InputT m (Either Text (Maybe Command))
getCommand = handleInterrupt (return (Right Nothing)) $ do
  line <- withInterrupt $ getInputLine "> "
  case line of
    Nothing -> return (Right (Just QuitPSCi)) -- Ctrl-D when input is empty
    Just "" -> return (Right Nothing)
    Just s  -> return . fmap Just $ parseCommand $ toS s

pasteMode :: forall m. MonadException m => InputT m (Either Text Command)
pasteMode =
    parseCommand <$> go []
  where
    go :: [Text] -> InputT m Text
    go ls = maybe (return . T.unlines $ reverse ls) (go . (:ls) . toS) =<< getInputLine "… "

-- | Make a JavaScript bundle for the browser.
bundle :: IO (Either Bundle.ErrorMessage Text)
bundle = runExceptT $ do
  inputFiles <- liftIO (glob (".psci_modules" </> "node_modules" </> "*" </> "*.js"))
  input <- for inputFiles $ \filename -> do
    mid <- Bundle.guessModuleIdentifier filename
    withUTF8FileContentsT filename (mid, )
  Bundle.bundle input [] Nothing "PSCI"

indexJS :: Text -- IsString string => string
indexJS = $(embedStringFile "app/static/index.js")

indexPage :: Text -- IsString string => string
indexPage = $(embedStringFile "app/static/index.html")

-- | All of the functions required to implement a PSCi backend
data Backend = forall state. Backend
  { _backendSetup :: IO state
  -- ^ Initialize, and call the continuation when the backend is ready
  , _backendEval :: state -> Text -> IO ()
  -- ^ Evaluate JavaScript code
  , _backendReload :: state -> IO ()
  -- ^ Reload the compiled code
  , _backendShutdown :: state -> IO ()
  -- ^ Shut down the backend
  }

-- | Commands which can be sent to the browser
data BrowserCommand
  = Eval (MVar Text)
  -- ^ Evaluate the latest JS
  | Refresh
  -- ^ Refresh the page

-- | State for the browser backend
data BrowserState = BrowserState
  { browserCommands       :: TChan BrowserCommand
  -- ^ A channel which receives data when the compiled JS has
  -- been updated
  , browserShutdownNotice :: MVar ()
  -- ^ An MVar which becomes full when the server should shut down
  , browserIndexJS        :: TVar (Maybe Text)
  -- ^ A TVar holding the latest compiled JS
  , browserBundleJS       :: TVar (Maybe Text)
  -- ^ A TVar holding the latest bundled JS
  }

browserBackend :: Int -> Backend
browserBackend serverPort = Backend setup evaluate reload shutdown
  where
    setup :: IO BrowserState
    setup = do
      shutdownVar <- newEmptyMVar
      cmdChan <- newBroadcastTChanIO
      indexJs <- newTVarIO Nothing :: IO (TVar (Maybe Text))
      bundleJs <- newTVarIO Nothing :: IO (TVar (Maybe Text))

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
                tryPutMVar resultVar result
              Refresh ->
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
                                        [(hContentType, "text/html; charset=UTF-8")]
                                        (toS indexPage)
            ["js", "index.js"] ->
              respond $ Wai.responseLBS status200
                                        [(hContentType, "application/javascript")]
                                        (toS indexJS)
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
                                            (toS js)
            ["js", "bundle.js"] -> do
              may <- readTVarIO bundleJs
              case may of
                Nothing ->
                  respond $ Wai.responseLBS status503 [] "Service not available"
                Just js ->
                  respond $ Wai.responseLBS status200
                                            [ (hContentType, "application/javascript")]
                                            (toS js)
            _ -> respond $ Wai.responseLBS status404 [] "Not found"

      let browserState = BrowserState cmdChan shutdownVar indexJs bundleJs
      createBundle browserState
      putText $ "Serving http://localhost:" <> show serverPort <> "/. Waiting for connections..."
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
      putText "Bundling JavaScript..."
      ejs <- bundle
      case ejs of
        Left err -> do
          putTextLines (Bundle.printErrorMessage err)
          exitFailure
        Right js ->
          atomically $ writeTVar (browserBundleJS state) (Just js)

    reload :: BrowserState -> IO ()
    reload state = do
      createBundle state
      atomically $ writeTChan (browserCommands state) Refresh

    shutdown :: BrowserState -> IO ()
    shutdown state = putMVar (browserShutdownNotice state) ()

    evaluate :: BrowserState -> Text -> IO ()
    evaluate state js = liftIO $ do
      resultVar <- newEmptyMVar
      atomically $ do
        writeTVar (browserIndexJS state) (Just js)
        writeTChan (browserCommands state) (Eval resultVar)
      result <- takeMVar resultVar
      putStrLn result

nodeBackend :: Maybe FilePath -> [Text] -> Backend
nodeBackend nodePath nodeArgs = Backend setup eval reload shutdown
  where
    setup :: IO ()
    setup = return ()

    eval :: () -> Text -> IO ()
    eval _ _ = do
      writeUTF8FileT indexFile "require('$PSCI')['$main']();"
      process <- maybe findNodeProcess (pure . pure) nodePath
      result <- traverse (\node -> readProcessWithExitCode node (map toS nodeArgs <> [indexFile]) "") process
      case result of
        Just (ExitSuccess, out, _)   -> putStrLn out
        Just (ExitFailure _, _, err) -> putStrLn err
        Nothing                      -> putText "Couldn't find node.js"

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
        inputFiles <- concat <$> traverse glob psciInputGlob
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
              Left errs -> do
                pwd <- getCurrentDirectory
                putStrLn (P.prettyPrintMultipleErrors P.defaultPPEOptions {P.ppeRelativeDirectory = pwd} errs) >> exitFailure
              Right (modules, externs, env) -> do
                historyFilename <- getHistoryFilename
                let settings = defaultSettings { historyFile = Just historyFilename }
                    initialState = PSCiState [] [] (zip (map snd modules) externs)
                    config = PSCiConfig psciInputGlob env
                    runner = flip runReaderT config
                             . flip evalStateT initialState
                             . runInputT (setComplete completion settings)

                    handleCommand' :: state -> Command -> StateT PSCiState (ReaderT PSCiConfig IO) ()
                    handleCommand' state = handleCommand (liftIO . eval state) (liftIO (reload state)) (liftIO . putStrLn)

                    go :: state -> InputT (StateT PSCiState (ReaderT PSCiConfig IO)) ()
                    go state = do
                      c <- getCommand
                      case c of
                        Left err -> out err >> go state
                        Right Nothing -> go state
                        Right (Just PasteLines) -> do
                          c' <- pasteMode
                          case c' of
                            Left err -> out err >> go state
                            Right c'' -> handleCommandWithInterrupts state c''
                        Right (Just QuitPSCi) -> do
                          out quitMessage
                          liftIO $ shutdown state
                        Right (Just c') -> handleCommandWithInterrupts state c'
                        where out = outputStrLn . toS

                    loadUserConfig :: state -> StateT PSCiState (ReaderT PSCiConfig IO) ()
                    loadUserConfig state = do
                      configFile <- (</> ".purs-repl") <$> liftIO getCurrentDirectory
                      exists <- liftIO $ doesFileExist configFile
                      let runner' = flip runReaderT config . flip evalStateT initialState
                      when exists $ runWithUTF8FileContentsT runner' configFile $ \contents -> do
                        for_ (T.lines contents) $ \l -> do
                          liftIO (putStrLn l)
                          case parseCommand l of
                            Left err -> liftIO (putStrLn err >> exitFailure)
                            Right cmd@Import{} -> handleCommand' state cmd
                            Right _ -> liftIO (putText "The .purs-repl file only supports import declarations")

                    handleCommandWithInterrupts
                      :: state
                      -> Command
                      -> InputT (StateT PSCiState (ReaderT PSCiConfig IO)) ()
                    handleCommandWithInterrupts state cmd = do
                      handleInterrupt (outputStrLn "Interrupted.")
                                      (withInterrupt (lift (handleCommand' state cmd)))
                      go state

                putStrLn prologueMessage
                backendState <- setup
                runner (lift (loadUserConfig backendState) >> go backendState)
