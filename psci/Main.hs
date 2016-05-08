{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import           Prelude ()
import           Prelude.Compat

import           Data.List (isPrefixOf, sortBy)
import           Data.Maybe (mapMaybe)
import           Data.Monoid ((<>))
import           Data.Version (showVersion)

import           Control.Applicative (many)
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.State.Strict (StateT, evalStateT)

import qualified Language.PureScript as P
import           Language.PureScript.Interactive
import qualified Language.PureScript.Interactive.Directive as D

import qualified Options.Applicative as Opts

import qualified Paths_purescript as Paths

import           System.Console.Haskeline
import           System.Exit
import           System.FilePath.Glob (glob)
import           System.IO.UTF8 (readUTF8File)

multiLineMode :: Opts.Parser Bool
multiLineMode = Opts.switch $
     Opts.long "multi-line-mode"
  <> Opts.short 'm'
  <> Opts.help "Run in multi-line mode (use ^D to terminate commands)"

inputFile :: Opts.Parser FilePath
inputFile = Opts.strArgument $
     Opts.metavar "FILE"
  <> Opts.help "Optional .purs files to load on start"

inputForeignFile :: Opts.Parser FilePath
inputForeignFile = Opts.strOption $
     Opts.short 'f'
  <> Opts.long "ffi"
  <> Opts.help "The input .js file(s) providing foreign import implementations"

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
                          <*> many inputForeignFile
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

-- | Read a file in the 'P.Make' monad, handling errors.
readFileMake :: FilePath -> P.Make String
readFileMake path = P.makeIO (const (P.ErrorMessage [] $ P.CannotReadFile path)) (readUTF8File path)

-- | Parses the input and returns either a command, or an error as a 'String'.
getCommand :: Bool -> InputT (StateT PSCiState IO) (Either String (Maybe Command))
getCommand singleLineMode = handleInterrupt (return (Right Nothing)) $ do
  firstLine <- withInterrupt $ getInputLine "> "
  case firstLine of
    Nothing -> return (Right (Just QuitPSCi)) -- Ctrl-D when input is empty
    Just "" -> return (Right Nothing)
    Just s | singleLineMode || head s == ':' -> return .fmap Just $ parseCommand s
    Just s -> fmap Just . parseCommand <$> go [s]
  where
    go :: [String] -> InputT (StateT PSCiState IO) String
    go ls = maybe (return . unlines $ reverse ls) (go . (:ls)) =<< getInputLine "  "

-- | Checks if the Console module is defined
consoleIsDefined :: [P.ExternsFile] -> Bool
consoleIsDefined = any ((== P.ModuleName (map P.ProperName [ "Control", "Monad", "Eff", "Console" ])) . P.efModuleName)

-- | Loads module, function, and file completions.
completion :: CompletionFunc (StateT PSCiState IO)
completion = liftCompletionM . completion'

completion' :: CompletionFunc CompletionM
completion' = completeWordWithPrev Nothing " \t\n\r" findCompletions

getCompletion :: CompletionContext -> CompletionM [Either String Completion]
getCompletion ctx =
  case ctx of
    CtxFilePath f        -> map Right <$> listFiles f
    CtxModule            -> map Left <$> getModuleNames
    CtxIdentifier        -> map Left <$> ((++) <$> getIdentNames <*> getDctorNames)
    CtxType              -> map Left <$> getTypeNames
    CtxFixed str         -> return [Left str]
    CtxDirective d       -> return (map Left (completeDirectives d))

  where
  completeDirectives :: String -> [String]
  completeDirectives = map (':' :) . D.directiveStringsFor

-- | Callback for Haskeline's `completeWordWithPrev`.
-- Expects:
--   * Line contents to the left of the word, reversed
--   * Word to be completed
findCompletions :: String -> String -> CompletionM [Completion]
findCompletions prev word = do
    let ctx = completionContext (words (reverse prev)) word
    completions <- concat <$> traverse getCompletions ctx
    return $ sortBy directivesFirst completions
  where
    getCompletions :: CompletionContext -> CompletionM [Completion]
    getCompletions = fmap (mapMaybe (either (prefixedBy word) Just)) . getCompletion

    prefixedBy :: String -> String -> Maybe Completion
    prefixedBy w cand = if w `isPrefixOf` cand
                          then Just (simpleCompletion cand)
                          else Nothing

    directivesFirst :: Completion -> Completion -> Ordering
    directivesFirst (Completion _ d1 _) (Completion _ d2 _) = go d1 d2
      where
      go (':' : xs) (':' : ys) = compare xs ys
      go (':' : _) _ = LT
      go _ (':' : _) = GT
      go xs ys = compare xs ys

-- | Get command line options and drop into the REPL
main :: IO ()
main = getOpt >>= loop
  where
    loop :: PSCiOptions -> IO ()
    loop PSCiOptions{..} = do
        inputFiles <- concat <$> traverse glob psciInputFile
        foreignFiles <- concat <$> traverse glob psciForeignInputFiles
        e <- runExceptT $ do
          modules <- ExceptT (loadAllModules inputFiles)
          let allModules = ("<internal>", supportModule) : modules
          foreigns <- ExceptT . runMake $ do
            foreignFilesContent <- forM foreignFiles (\inFile -> (inFile,) <$> readFileMake inFile)
            P.parseForeignModulesFromFiles foreignFilesContent
          (externs, env) <- ExceptT . runMake . make (initialPSCiState { psciForeignFiles = foreigns }) . map snd $ allModules
          return (allModules, foreigns, externs, env)
        case e of
          Left errs -> putStrLn (P.prettyPrintMultipleErrors False errs) >> exitFailure
          Right (modules, foreigns, externs, env) -> do
            historyFilename <- getHistoryFilename
            let settings = defaultSettings { historyFile = Just historyFilename }
            flip evalStateT (PSCiState [] inputFiles (zip (map snd modules) externs) foreigns [] psciInputNodeFlags env) . runInputT (setComplete completion settings) $ do
              outputStrLn prologueMessage
              unless (consoleIsDefined externs) . outputStrLn $ unlines
                [ "PSCi requires the purescript-console module to be installed."
                , "For help getting started, visit http://wiki.purescript.org/PSCi"
                ]
              go
      where
        go :: InputT (StateT PSCiState IO) ()
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
