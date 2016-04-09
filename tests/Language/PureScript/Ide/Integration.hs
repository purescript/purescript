-----------------------------------------------------------------------------
--
-- Module      : Language.PureScript.Ide.Integration
-- Description : A psc-ide client for use in integration tests
-- Copyright   : Christoph Hegemann 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- A psc-ide client for use in integration tests
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.PureScript.Ide.Integration
       (
         -- managing the server process
         startServer
       , withServer
       , stopServer
       , quitServer
         -- util
       , compileTestProject
       , deleteOutputFolder
       , projectDirectory
       , deleteFileIfExists
         -- sending commands
       , loadModule
       , loadModuleWithDeps
       , getFlexCompletions
       , getType
       , addImport
       , addImplicitImport
       , rebuildModule
         -- checking results
       , resultIsSuccess
       , parseCompletions
       , parseTextResult
       ) where

import           Control.Concurrent                (threadDelay)
import           Control.Exception
import           Control.Monad                     (join, when)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.UTF8         as BSL
import           Data.Either                       (isRight)
import           Data.Maybe                        (fromJust)
import qualified Data.Text                         as T
import qualified Data.Vector                       as V
import           Language.PureScript.Ide.Util
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process

projectDirectory :: IO FilePath
projectDirectory = do
  cd <- getCurrentDirectory
  return $ cd </> "tests" </> "support" </> "pscide"

startServer :: IO ProcessHandle
startServer = do
  pdir <- projectDirectory
  (_, _, _, procHandle) <- createProcess $ (shell "psc-ide-server") {cwd=Just pdir}
  threadDelay 500000 -- give the server 500ms to start up
  return procHandle

stopServer :: ProcessHandle -> IO ()
stopServer = terminateProcess

withServer :: IO a -> IO a
withServer s = do
  _ <- startServer
  r <- s
  quitServer
  return r

-- project management utils

compileTestProject :: IO Bool
compileTestProject = do
  pdir <- projectDirectory
  (_, _, _, procHandle) <- createProcess $
    (shell $ "psc " ++ fileGlob) {cwd=Just pdir
                                 ,std_out=CreatePipe
                                 ,std_err=CreatePipe
                                 }
  isSuccess <$> waitForProcess procHandle

deleteOutputFolder :: IO ()
deleteOutputFolder = do
  odir <- fmap (</> "output") projectDirectory
  whenM (doesDirectoryExist odir) (removeDirectoryRecursive odir)

deleteFileIfExists :: FilePath -> IO ()
deleteFileIfExists fp = whenM (doesFileExist fp) (removeFile fp)

whenM :: Monad m => m Bool -> m () -> m ()
whenM p f = do
  x <- p
  when x f

isSuccess :: ExitCode -> Bool
isSuccess ExitSuccess = True
isSuccess (ExitFailure _) = False

fileGlob :: String
fileGlob = unwords
  [ "\"src/**/*.purs\""
  , "\"src/**/*.js\""
  , "\"bower_components/purescript-*/**/*.purs\""
  , "\"bower_components/purescript-*/**/*.js\""
  ]

-- Integration Testing API

sendCommand :: Value -> IO String
sendCommand v = readCreateProcess
  ((shell "psc-ide-client") { std_out=CreatePipe
                            , std_err=CreatePipe
                            })
  (T.unpack (encodeT v))

quitServer :: IO ()
quitServer = do
  let quitCommand = object ["command" .= ("quit" :: String)]
  _ <- try $ sendCommand quitCommand :: IO (Either SomeException String)
  return ()

loadModuleWithDeps :: String -> IO String
loadModuleWithDeps m = sendCommand $ load [] [m]

loadModule :: String -> IO String
loadModule m = sendCommand $ load [m] []

getFlexCompletions :: String -> IO [(String, String, String)]
getFlexCompletions q = parseCompletions <$> sendCommand (completion [] (Just (flexMatcher q)))

getType :: String -> IO [(String, String, String)]
getType q = parseCompletions <$> sendCommand (typeC q [])

addImport :: String -> FilePath -> FilePath -> IO String
addImport identifier fp outfp = sendCommand (addImportC identifier fp outfp)

addImplicitImport :: String -> FilePath -> FilePath -> IO String
addImplicitImport mn fp outfp = sendCommand (addImplicitImportC mn fp outfp)

rebuildModule :: FilePath -> IO String
rebuildModule m = sendCommand (rebuildC m Nothing)

-- Command Encoding

commandWrapper :: String -> Value -> Value
commandWrapper c p = object ["command" .= c, "params" .= p]

load :: [String] -> [String] -> Value
load ms ds = commandWrapper "load" (object ["modules" .= ms, "dependencies" .= ds])

typeC :: String -> [Value] -> Value
typeC q filters = commandWrapper "type" (object ["search" .= q, "filters" .= filters])

addImportC :: String -> FilePath -> FilePath -> Value
addImportC identifier = addImportW $
  object [ "importCommand" .= ("addImport" :: String)
         , "identifier" .= identifier
         ]

addImplicitImportC :: String -> FilePath -> FilePath -> Value
addImplicitImportC mn = addImportW $
  object [ "importCommand" .= ("addImplicitImport" :: String)
         , "module" .= mn
         ]

rebuildC :: FilePath -> Maybe FilePath -> Value
rebuildC file outFile =
  commandWrapper "rebuild" (object [ "file" .= file
                                   , "outfile" .= outFile
                                   ])

addImportW :: Value -> FilePath -> FilePath -> Value
addImportW importCommand fp outfp =
  commandWrapper "import" (object [ "file" .= fp
                                  , "outfile" .= outfp
                                  , "importCommand" .= importCommand
                                  ])


completion :: [Value] -> Maybe Value -> Value
completion filters matcher =
  let
    matcher' = case matcher of
      Nothing -> []
      Just m -> ["matcher" .= m]
  in
    commandWrapper "complete" (object $ "filters" .= filters : matcher')

flexMatcher :: String -> Value
flexMatcher q = object [ "matcher" .= ("flex" :: String)
                       , "params" .= object ["search" .= q]
                       ]

-- Result parsing

unwrapResult :: Value -> Parser (Either String Value)
unwrapResult = withObject "result" $ \o -> do
  (rt :: String) <- o .: "resultType"
  case rt of
    "error" -> do
      res <- o .: "result"
      pure (Left res)
    "success" -> do
      res <- o .: "result"
      pure (Right res)
    _ -> fail "lol"

withResult :: (Value -> Parser a) -> Value -> Parser (Either String a)
withResult p v = do
  r <- unwrapResult v
  case r of
    Left err -> pure (Left err)
    Right res -> Right <$> p res

completionParser :: Value -> Parser [(String, String, String)]
completionParser = withArray "res" $ \cs ->
  mapM (withObject "completion" $ \o -> do
           ident <- o .: "identifier"
           module' <- o .: "module"
           ty <- o .: "type"
           pure (module', ident, ty)) (V.toList cs)

valueFromString :: String -> Value
valueFromString = fromJust . decode . BSL.fromString

resultIsSuccess :: String -> Bool
resultIsSuccess = isRight . join . parseEither unwrapResult . valueFromString

parseCompletions :: String -> [(String, String, String)]
parseCompletions s = fromJust $ do
  cs <- parseMaybe (withResult completionParser) (valueFromString s)
  case cs of
    Left _ -> error "Failed to parse completions"
    Right cs' -> pure cs'

parseTextResult :: String -> String
parseTextResult s = fromJust $ do
  r <- parseMaybe (withResult (withText "tr" pure)) (valueFromString s)
  case r of
    Left _ -> error "Failed to parse textResult"
    Right r' -> pure (T.unpack r')
