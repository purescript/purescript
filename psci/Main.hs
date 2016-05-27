{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import           Prelude ()
import           Prelude.Compat

import           Data.Monoid ((<>))
import           Data.Version (showVersion)

import           Control.Applicative (many)
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.State.Strict (StateT, evalStateT)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)

import qualified Language.PureScript as P
import           Language.PureScript.Interactive

import qualified Options.Applicative as Opts

import qualified Paths_purescript as Paths

import           System.Console.Haskeline
import           System.Exit
import           System.FilePath.Glob (glob)

-- | Command line options
data PSCiOptions = PSCiOptions
  { psciMultiLineMode     :: Bool
  , psciInputFile         :: [FilePath]
  , psciInputNodeFlags    :: [String]
  }

multiLineMode :: Opts.Parser Bool
multiLineMode = Opts.switch $
     Opts.long "multi-line-mode"
  <> Opts.short 'm'
  <> Opts.help "Run in multi-line mode (use ^D to terminate commands)"

inputFile :: Opts.Parser FilePath
inputFile = Opts.strArgument $
     Opts.metavar "FILE"
  <> Opts.help "Optional .purs files to load on start"

nodeFlagsFlag :: Opts.Parser [String]
nodeFlagsFlag = Opts.option parser $
     Opts.long "node-opts"
  <> Opts.metavar "NODE_OPTS"
  <> Opts.value []
  <> Opts.help "Flags to pass to node, separated by spaces"
  where
    parser = words <$> Opts.str

psciOptions :: Opts.Parser PSCiOptions
psciOptions = PSCiOptions <$> multiLineMode
                          <*> many inputFile
                          <*> nodeFlagsFlag

version :: Opts.Parser (a -> a)
version = Opts.abortOption (Opts.InfoMsg (showVersion Paths.version)) $
            Opts.long "version" <>
            Opts.help "Show the version number" <>
            Opts.hidden

getOpt :: IO PSCiOptions
getOpt = Opts.execParser opts
    where
      opts        = Opts.info (version <*> Opts.helper <*> psciOptions) infoModList
      infoModList = Opts.fullDesc <> headerInfo <> footerInfo
      headerInfo  = Opts.header   "psci - Interactive mode for PureScript"
      footerInfo  = Opts.footer $ "psci " ++ showVersion Paths.version

-- | Parses the input and returns either a command, or an error as a 'String'.
getCommand :: forall m. MonadException m => Bool -> InputT m (Either String (Maybe Command))
getCommand singleLineMode = handleInterrupt (return (Right Nothing)) $ do
  firstLine <- withInterrupt $ getInputLine "> "
  case firstLine of
    Nothing -> return (Right (Just QuitPSCi)) -- Ctrl-D when input is empty
    Just "" -> return (Right Nothing)
    Just s | singleLineMode || head s == ':' -> return . fmap Just $ parseCommand s
    Just s -> fmap Just . parseCommand <$> go [s]
  where
    go :: [String] -> InputT m String
    go ls = maybe (return . unlines $ reverse ls) (go . (:ls)) =<< getInputLine "  "

-- | Get command line options and drop into the REPL
main :: IO ()
main = getOpt >>= loop
  where
    loop :: PSCiOptions -> IO ()
    loop PSCiOptions{..} = do
        inputFiles <- concat <$> traverse glob psciInputFile
        e <- runExceptT $ do
          modules <- ExceptT (loadAllModules inputFiles)
          unless (supportModuleIsDefined (map snd modules)) . liftIO $ do
            putStrLn supportModuleMessage
            exitFailure
          (externs, env) <- ExceptT . runMake . make $ modules
          return (modules, externs, env)
        case e of
          Left errs -> putStrLn (P.prettyPrintMultipleErrors P.defaultPPEOptions errs) >> exitFailure
          Right (modules, externs, env) -> do
            historyFilename <- getHistoryFilename
            let settings = defaultSettings { historyFile = Just historyFilename }
                initialState = PSCiState [] [] (zip (map snd modules) externs)
                config = PSCiConfig inputFiles psciInputNodeFlags env
                runner = flip runReaderT config
                         . flip evalStateT initialState
                         . runInputT (setComplete completion settings)
            putStrLn prologueMessage
            runner go
      where
        go :: InputT (StateT PSCiState (ReaderT PSCiConfig IO)) ()
        go = do
          c <- getCommand (not psciMultiLineMode)
          case c of
            Left err -> outputStrLn err >> go
            Right Nothing -> go
            Right (Just QuitPSCi) -> outputStrLn quitMessage
            Right (Just c') -> do
              handleInterrupt (outputStrLn "Interrupted.")
                              (withInterrupt (lift (handleCommand c')))
              go
