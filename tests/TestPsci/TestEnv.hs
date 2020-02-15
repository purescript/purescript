{-# LANGUAGE OverloadedStrings #-}

module TestPsci.TestEnv where

import Prelude ()
import Prelude.Compat

import           Control.Exception.Lifted (bracket_)
import           Control.Monad (forM, void, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.RWS.Strict (evalRWST, asks, local, RWST)
import           Data.Foldable (traverse_)
import           Data.List (isSuffixOf)
import qualified Data.Text as T
import qualified Language.PureScript as P
import qualified Language.PureScript.CST as CST
import           Language.PureScript.Interactive
import           System.Directory (getCurrentDirectory, doesPathExist, removeFile)
import           System.Exit
import           System.FilePath ((</>), pathSeparator)
import qualified System.FilePath.Glob as Glob
import           System.Process (readProcessWithExitCode)
import           Test.Hspec (shouldBe, Expectation)

-- | A monad transformer for handle PSCi actions in tests
type TestPSCi a = RWST PSCiConfig () PSCiState IO a

-- | Initialise PSCi state and config for tests
initTestPSCiEnv :: IO (PSCiState, PSCiConfig)
initTestPSCiEnv = do
  -- Load test support packages
  cwd <- getCurrentDirectory
  let supportDir = cwd </> "tests" </> "support"
  psciFiles <- Glob.globDir1 (Glob.compile "**/*.purs") (supportDir </> "psci")
  libraries <- Glob.globDir1 (Glob.compile "purescript-*/src/**/*.purs") (supportDir </> "bower_components")
  let pursFiles = psciFiles ++ libraries
  modulesOrError <- loadAllModules pursFiles
  case modulesOrError of
    Left err ->
      print err >> exitFailure
    Right modules -> do
      -- Make modules
      makeResultOrError <- runMake . make $ fmap CST.pureResult <$> modules
      case makeResultOrError of
        Left errs -> putStrLn (P.prettyPrintMultipleErrors P.defaultPPEOptions errs) >> exitFailure
        Right (externs, _) ->
          return (updateLoadedExterns (const (zip (map snd modules) externs)) initialPSCiState, PSCiConfig pursFiles)

-- | Execute a TestPSCi, returning IO
execTestPSCi :: TestPSCi a -> IO a
execTestPSCi i = do
  (s, c) <- initTestPSCiEnv -- init state and config
  fst <$> evalRWST i c s

-- | Evaluate JS to which a PSCi input is compiled. The actual JS input is not
-- needed as an argument, as it is already written in the file during the
-- command evaluation.
jsEval :: TestPSCi String
jsEval = liftIO $ do
  writeFile indexFile "require('$PSCI')['$main']();"
  process <- findNodeProcess
  result <- forM process $ \node -> do
    cwd <- getCurrentDirectory
    let esm = cwd </> "tests" </> "support" </> "node_modules" </> "esm"
    readProcessWithExitCode node ["--require", esm, indexFile] ""
  case result of
    Just (ExitSuccess, out, _)   -> return out
    Just (ExitFailure _, _, err) -> putStrLn err >> exitFailure
    Nothing                      -> putStrLn "Couldn't find node.js" >> exitFailure

-- | Run a PSCi command and evaluate its outputs:
-- * jsOutputEval is used to evaluate compiled JS output by PSCi
-- * printedOutputEval is used to evaluate text printed directly by PSCi itself
runAndEval :: String -> TestPSCi () -> (String -> TestPSCi ()) -> TestPSCi ()
runAndEval comm jsOutputEval textOutputEval =
  case parseCommand comm of
    Left errStr -> liftIO $ putStrLn errStr >> exitFailure
    Right commands ->
      -- The JS result is ignored, as it's already written in a JS source file.
      -- For the detail, please refer to Interactive.hs
      traverse_ (handleCommand (\_ -> jsOutputEval) (return ()) textOutputEval) commands

-- | Run a PSCi command, evaluate compiled JS, and ignore evaluation output and printed output
run :: String -> TestPSCi ()
run comm = runAndEval comm (void jsEval) ignorePrinted
  where
    ignorePrinted _ = return ()

-- | A lifted evaluation of Hspec 'shouldBe' for the TestPSCi
equalsTo :: (Eq a, Show a) => a -> a -> TestPSCi ()
equalsTo x y = liftIO $ x `shouldBe` y

-- | An assertion to check command evaluated javascript output against a given string
evaluatesTo :: String -> String -> TestPSCi ()
evaluatesTo command expected = runAndEval command evalJsAndCompare ignorePrinted
  where
    evalJsAndCompare = do
      actual <- jsEval
      actual `equalsTo` (expected ++ "\n")
    ignorePrinted _ = return ()

-- | An assertion to check command PSCi printed output against a given string
prints :: String -> String -> TestPSCi ()
prints command expected = printed command (`shouldBe` expected)

printed :: String -> (String -> Expectation) -> TestPSCi ()
printed command f = runAndEval command (void jsEval) (liftIO . f)

simulateModuleEdit :: P.ModuleName -> FilePath -> TestPSCi a -> TestPSCi a
simulateModuleEdit mn newPath action = do
  ms <- asks psciFileGlobs
  case replacePath ms of
    Nothing  -> fail $ "Did not find " ++ inputPath ++ " in psciFileGlobs"
    Just xs' -> local (\c -> c { psciFileGlobs = xs' }) temporarily <* rebuild

  where
  outputPath = modulesDir </> T.unpack (P.runModuleName mn) </> "index.js"
  inputPath  = T.unpack (T.replace "." slash (P.runModuleName mn)) ++ ".purs"
  slash      = T.singleton pathSeparator

  replacePath :: [String] -> Maybe [String]
  replacePath (x:xs)
    | inputPath `isSuffixOf` x = Just (newPath : xs)
    | otherwise                = fmap (x:) (replacePath xs)
  replacePath []               = Nothing

  -- Simply adding the file to `PSCiConfig.fileGlobs` isn't sufficient; running
  -- ":reload" might not rebuild because the compiled JS artifact has a more
  -- recent timestamp than the "new" source file `newPath`.
  temporarily   = bracket_ enableRebuild enableRebuild action
  enableRebuild = liftIO $ do { b <- doesPathExist outputPath; when b (removeFile outputPath) }
  rebuild       = handleCommand discard (return ()) discard ReloadState
  discard _     = return ()
