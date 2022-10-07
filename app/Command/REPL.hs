{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GADTs #-}

module Command.REPL (command) where

import           Prelude
import           Control.Applicative (many, (<|>))
import           Control.Monad
import           Control.Monad.Catch (MonadMask)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.State.Strict (StateT, evalStateT)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Data.Foldable (for_)
import qualified Language.PureScript as P
import qualified Language.PureScript.CST as CST
import           Language.PureScript.Interactive
import qualified Options.Applicative as Opts
import           System.Console.Haskeline
import           System.IO.UTF8 (readUTF8File)
import           System.Exit
import           System.Directory (doesFileExist, getCurrentDirectory)
import           System.FilePath ((</>))
import qualified System.FilePath.Glob as Glob
import System.IO (hPutStrLn, stderr)

-- | Command line options
data PSCiOptions = PSCiOptions
  { psciInputGlob         :: [String]
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
  <> Opts.help "The browser REPL backend was removed in v0.15.0. Use https://try.purescript.org instead."

backend :: Opts.Parser Backend
backend =
  (browserBackend <$> port)
  <|> (nodeBackend <$> nodePathOption <*> nodeFlagsOption)

psciOptions :: Opts.Parser PSCiOptions
psciOptions = PSCiOptions <$> many inputFile
                          <*> backend

-- | Parses the input and returns either a command, or an error as a 'String'.
getCommand :: forall m. (MonadIO m, MonadMask m) => InputT m (Either String [Command])
getCommand = handleInterrupt (return (Right [])) $ do
  line <- withInterrupt $ getInputLine "> "
  case line of
    Nothing -> return (Right [QuitPSCi]) -- Ctrl-D when input is empty
    Just "" -> return (Right [])
    Just s  -> return (parseCommand s)

pasteMode :: forall m. (MonadIO m, MonadMask m) => InputT m (Either String [Command])
pasteMode =
    parseCommand <$> go []
  where
    go :: [String] -> InputT m String
    go ls = maybe (return . unlines $ reverse ls) (go . (:ls)) =<< getInputLine "… "

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

browserBackend :: Int -> Backend
browserBackend _ = Backend setup mempty mempty mempty
  where
    setup :: IO ()
    setup = do
      hPutStrLn stderr "The browser REPL backend was removed in v0.15.0. Use TryPureScript instead: https://try.purescript.org"
      exitFailure

nodeBackend :: Maybe FilePath -> [String] -> Backend
nodeBackend nodePath nodeArgs = Backend setup eval reload shutdown
  where
    setup :: IO ()
    setup = return ()

    eval :: () -> String -> IO ()
    eval _ _ = do
      writeFile indexFile "import('./$PSCI/index.js').then(({ $main }) => $main());"
      result <- readNodeProcessWithExitCode nodePath (nodeArgs ++ [indexFile]) ""
      case result of
        Right (ExitSuccess, out, _)   -> putStrLn out
        Right (ExitFailure _, _, err) -> putStrLn err
        Left err                      -> putStrLn err

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
        inputFiles <- concat <$> traverse Glob.glob psciInputGlob
        e <- runExceptT $ do
          modules <- ExceptT (loadAllModules inputFiles)
          when (null modules) . liftIO $ do
            putStr noInputMessage
            exitFailure
          unless (supportModuleIsDefined (map (P.getModuleName . snd) modules)) . liftIO $ do
            putStr supportModuleMessage
            exitFailure
          (externs, _) <- ExceptT . runMake . make $ fmap CST.pureResult <$> modules
          return (modules, externs)
        case psciBackend of
          Backend setup eval reload (shutdown :: state -> IO ()) ->
            case e of
              Left errs -> do
                pwd <- getCurrentDirectory
                putStrLn (P.prettyPrintMultipleErrors P.defaultPPEOptions {P.ppeRelativeDirectory = pwd} errs) >> exitFailure
              Right (modules, externs) -> do
                historyFilename <- getHistoryFilename
                let settings = defaultSettings { historyFile = Just historyFilename }
                    initialState = updateLoadedExterns (const (zip (map snd modules) externs)) initialPSCiState
                    config = PSCiConfig psciInputGlob
                    runner = flip runReaderT config
                             . flip evalStateT initialState
                             . runInputT (setComplete completion settings)

                    handleCommand' :: state -> Command -> StateT PSCiState (ReaderT PSCiConfig IO) ()
                    handleCommand' state = handleCommand (liftIO . eval state) (liftIO (reload state)) (liftIO . putStrLn)

                    go :: state -> InputT (StateT PSCiState (ReaderT PSCiConfig IO)) ()
                    go state = do
                      c <- getCommand
                      case c of
                        Left err -> outputStrLn err >> go state
                        Right xs -> goExec xs
                      where
                      goExec :: [Command] -> InputT (StateT PSCiState (ReaderT PSCiConfig IO)) ()
                      goExec xs = case xs of
                        [] -> go state
                        (PasteLines : rest) -> do
                          c' <- pasteMode
                          case c' of
                            Left err -> outputStrLn err >> goExec rest
                            Right c'' -> handleCommandWithInterrupts state c'' >> goExec rest
                        (QuitPSCi : _) -> do
                          outputStrLn quitMessage
                          liftIO $ shutdown state
                        (c' : rest) -> handleCommandWithInterrupts state [c'] >> goExec rest

                    loadUserConfig :: state -> StateT PSCiState (ReaderT PSCiConfig IO) ()
                    loadUserConfig state = do
                      configFile <- (</> ".purs-repl") <$> liftIO getCurrentDirectory
                      exists <- liftIO $ doesFileExist configFile
                      when exists $ do
                        cf <- liftIO (readUTF8File configFile)
                        case parseDotFile configFile cf of
                          Left err -> liftIO (putStrLn err >> exitFailure)
                          Right cmds -> liftIO (putStrLn cf) >> for_ cmds (handleCommand' state)

                    handleCommandWithInterrupts
                      :: state
                      -> [Command]
                      -> InputT (StateT PSCiState (ReaderT PSCiConfig IO)) ()
                    handleCommandWithInterrupts state cmds = do
                      handleInterrupt (outputStrLn "Interrupted.")
                                      (withInterrupt (lift (for_ cmds (handleCommand' state))))

                putStrLn prologueMessage
                backendState <- setup
                runner (lift (loadUserConfig backendState) >> go backendState)
