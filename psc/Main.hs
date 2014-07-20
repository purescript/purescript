-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
module Main where

import Control.Applicative
import Control.Monad.Error

import Data.Version (showVersion)
import Data.List
import System.Console.CmdTheLine
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath (takeDirectory)
import System.Exit (exitSuccess, exitFailure)
import System.IO (stderr)

--import Text.Parsec 
import Text.Parsec.Error
import Text.Printf
import Text.Parsec.Pos 

import qualified Language.PureScript as P
import qualified Language.PureScript.DevTools.Project as PRJ
import qualified Paths_purescript as Paths
import qualified System.IO.UTF8 as U

preludeFilename :: IO FilePath
preludeFilename = Paths.getDataFileName "prelude/prelude.purs"

readInput :: Maybe [FilePath] -> IO (Either ParseError [(FilePath, P.Module)])
readInput Nothing = do
  text <- getContents
  return $ map ((,) undefined) <$> P.runIndentParser "" P.parseModules text
readInput (Just input) = fmap collect $ forM input $ \inputFile -> do
  text <- U.readFile inputFile
  return $ (inputFile, P.runIndentParser inputFile P.parseModules text)
  where
  collect :: [(FilePath, Either ParseError [P.Module])] -> Either ParseError [(FilePath, P.Module)]
  collect = fmap concat . sequence . map (\(fp, e) -> fmap (map ((,) fp)) e)

jsonForParseError :: ParseError -> String
jsonForParseError pe =
  printf tpl n l c $ intercalate "," (map jsonForMessage ms)
  where ms = errorMessages pe
        sp = errorPos pe
        (n, l, c) = (sourceName sp, sourceLine sp, sourceColumn sp)
        tpl = "{\"type\":\"parse\", \"pos\" : { \"name\" : \"%s\", \"line\" : %d, \"col\" : %d}, \"messages\" : [%s]}"
        escapeString ('\\' : '"':xs) = '\\' : '"' : escapeString xs
        escapeString ('"':xs) = '\\' : '"' : escapeString xs
        escapeString (x:xs) = x : escapeString xs
        escapeString [] = []
        buildMessage t msg = "{\"type\":\"" ++ t ++ "\", \"msg\":\"" ++ (escapeString msg) ++ "\"}"
        jsonForMessage (SysUnExpect msg) = buildMessage "SysUnExpect" msg
        jsonForMessage (UnExpect msg) = buildMessage "UnExpect" msg
        jsonForMessage (Expect msg) = buildMessage "Expect" msg
        jsonForMessage (Message msg) = buildMessage "Message" msg

checkOnly :: P.Options -> Maybe [FilePath] -> Maybe FilePath -> Maybe FilePath -> IO ()
checkOnly opts input output externs = do
  putStrLn "**** Checking for errors"
  modules <- readInput input
  case modules of
    Left err -> do
      U.hPutStr stderr $ jsonForParseError err
      exitFailure
    Right ms -> do
      case P.checkOnly opts (map snd ms) of
        Left err -> do
          U.hPutStrLn stderr err
          exitFailure
        Right _x -> do
          print _x
          exitSuccess

compile :: P.Options -> Maybe [FilePath] -> Maybe FilePath -> Maybe FilePath -> IO ()
compile opts input output externs = do
  modules <- readInput input
  case modules of
    Left err -> do
      U.hPutStr stderr $ show err
      exitFailure
    Right ms -> do
      case P.compile opts (map snd ms) of
        Left err -> do
          U.hPutStrLn stderr err
          exitFailure
        Right (js, exts, _) -> do
          case output of
            Just path -> mkdirp path >> U.writeFile path js
            Nothing -> U.putStrLn js
          case externs of
            Just path -> mkdirp path >> U.writeFile path exts
            Nothing -> return ()
          exitSuccess

runCommand :: Bool -> P.Options -> Maybe [FilePath] -> Maybe FilePath -> Maybe FilePath -> IO ()
runCommand True opts (Just files) output externs = do
  cwd <- getCurrentDirectory
  files_ <- PRJ.filesForProject cwd
  let allFiles = Just $ files ++ files_
  print allFiles
  runCommand' opts allFiles output externs
runCommand _ opts files output externs = runCommand' opts files output externs

runCommand' :: P.Options -> Maybe [FilePath] -> Maybe FilePath -> Maybe FilePath -> IO ()
runCommand' opts files output externs =
  if P.optionsErrorCheck opts
    then checkOnly opts files output externs
    else compile opts files output externs

mkdirp :: FilePath -> IO ()
mkdirp = createDirectoryIfMissing True . takeDirectory

useStdIn :: Term Bool
useStdIn = value . flag $ (optInfo [ "s", "stdin" ])
     { optDoc = "Read from standard input" }

inputFiles :: Term [FilePath]
inputFiles = value $ posAny [] $ posInfo
     { posDoc = "The input .ps files" }

outputFile :: Term (Maybe FilePath)
outputFile = value $ opt Nothing $ (optInfo [ "o", "output" ])
     { optDoc = "The output .js file" }

externsFile :: Term (Maybe FilePath)
externsFile = value $ opt Nothing $ (optInfo [ "e", "externs" ])
     { optDoc = "The output .e.ps file" }

noTco :: Term Bool
noTco = value $ flag $ (optInfo [ "no-tco" ])
     { optDoc = "Disable tail call optimizations" }

performRuntimeTypeChecks :: Term Bool
performRuntimeTypeChecks = value $ flag $ (optInfo [ "runtime-type-checks" ])
     { optDoc = "Generate runtime type checks" }

noPrelude :: Term Bool
noPrelude = value $ flag $ (optInfo [ "no-prelude" ])
     { optDoc = "Omit the Prelude" }

noMagicDo :: Term Bool
noMagicDo = value $ flag $ (optInfo [ "no-magic-do" ])
     { optDoc = "Disable the optimization that overloads the do keyword to generate efficient code specifically for the Eff monad." }

runMain :: Term (Maybe String)
runMain = value $ defaultOpt (Just "Main") Nothing $ (optInfo [ "main" ])
     { optDoc = "Generate code to run the main method in the specified module." }

noOpts :: Term Bool
noOpts = value $ flag $ (optInfo [ "no-opts" ])
     { optDoc = "Skip the optimization phase." }

browserNamespace :: Term String
browserNamespace = value $ opt "PS" $ (optInfo [ "browser-namespace" ])
     { optDoc = "Specify the namespace that PureScript modules will be exported to when running in the browser." }

dceModules :: Term [String]
dceModules = value $ optAll [] $ (optInfo [ "m", "module" ])
     { optDoc = "Enables dead code elimination, all code which is not a transitive dependency of a specified module will be removed. This argument can be used multiple times." }

codeGenModules :: Term [String]
codeGenModules = value $ optAll [] $ (optInfo [ "codegen" ])
     { optDoc = "A list of modules for which Javascript and externs should be generated. This argument can be used multiple times." }

verboseErrors :: Term Bool
verboseErrors = value $ flag $ (optInfo [ "v", "verbose-errors" ])
     { optDoc = "Display verbose error messages" }

errorsCheck :: Term Bool
errorsCheck = value $ flag $ (optInfo [ "e", "errors-check" ])
     { optDoc = "Check for errors only" }

useProject :: Term Bool
useProject = value $ flag $ (optInfo ["p", "project"])
     { optDoc = "Include files from current project"}

options :: Term P.Options
options = P.Options <$> noPrelude <*> noTco <*> performRuntimeTypeChecks <*> noMagicDo <*> runMain <*> noOpts <*> (Just <$> browserNamespace) <*> dceModules <*> codeGenModules <*> verboseErrors <*> errorsCheck

stdInOrInputFiles :: FilePath -> Term (Maybe [FilePath])
stdInOrInputFiles prelude = combine <$> useStdIn <*> (not <$> noPrelude) <*> inputFiles
  where
  combine False True input = Just (prelude : input)
  combine False False input = Just input
  combine True _ _ = Nothing

term :: FilePath -> Term (IO ())
term prelude = runCommand <$> useProject <*> options <*> stdInOrInputFiles prelude <*> outputFile <*> externsFile

termInfo :: TermInfo
termInfo = defTI
  { termName = "psc"
  , version  = showVersion Paths.version
  , termDoc  = "Compiles PureScript to Javascript"
  }

main :: IO ()
main = do
  prelude <- preludeFilename
  run (term prelude, termInfo)
