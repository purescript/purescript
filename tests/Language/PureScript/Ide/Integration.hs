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
{-# LANGUAGE NoImplicitPrelude   #-}
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
       , addImport
       , addImplicitImport
       , loadAll
       , loadModule
       , loadModules
       , getCwd
       , getFlexCompletions
       , getFlexCompletionsInModule
       , getType
       , rebuildModule
       , reset
         -- checking results
       , resultIsSuccess
       , parseCompletions
       , parseTextResult
       ) where

import           Protolude
import           Data.Maybe                   (fromJust)

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text                    as T
import qualified Data.Vector                  as V
import           Language.PureScript.Ide.Util
import qualified Language.PureScript          as P
import           System.Directory
import           System.FilePath
import           System.IO.Error              (mkIOError, userErrorType)
import           System.Process

projectDirectory :: IO FilePath
projectDirectory = do
  cd <- getCurrentDirectory
  return $ cd </> "tests" </> "support" </> "pscide"

startServer :: IO ProcessHandle
startServer = do
  pdir <- projectDirectory
  -- Turn off filewatching since it creates race condition in a testing environment
  (_, _, _, procHandle) <- createProcess $
    (shell "psc-ide-server --no-watch src/*.purs") {cwd = Just pdir}
  threadDelay 2000000 -- give the server 2s to start up
  return procHandle

stopServer :: ProcessHandle -> IO ()
stopServer = terminateProcess

withServer :: IO a -> IO a
withServer s = do
  _ <- startServer
  started <- tryNTimes 5 (rightToMaybe <$> (try getCwd :: IO (Either SomeException Text)))
  when (isNothing started) $
    throwIO (mkIOError userErrorType "psc-ide-server didn't start in time" Nothing Nothing)
  r <- s
  quitServer
  pure r

-- project management utils

compileTestProject :: IO Bool
compileTestProject = do
  pdir <- projectDirectory
  (_, _, _, procHandle) <- createProcess $
    (shell . toS $ "psc " <> fileGlob) { cwd = Just pdir }
  r <- tryNTimes 10 (getProcessExitCode procHandle)
  pure (fromMaybe False (isSuccess <$> r))

tryNTimes :: Int -> IO (Maybe a) -> IO (Maybe a)
tryNTimes 0 _ = pure Nothing
tryNTimes n action = do
  r <- action
  case r of
    Nothing -> do
      threadDelay 500000
      tryNTimes (n - 1) action
    Just a -> pure (Just a)

deleteOutputFolder :: IO ()
deleteOutputFolder = do
  odir <- fmap (</> "output") projectDirectory
  whenM (doesDirectoryExist odir) (removeDirectoryRecursive odir)

deleteFileIfExists :: FilePath -> IO ()
deleteFileIfExists fp = whenM (doesFileExist fp) (removeFile fp)

isSuccess :: ExitCode -> Bool
isSuccess ExitSuccess = True
isSuccess (ExitFailure _) = False

fileGlob :: Text
fileGlob = "\"src/**/*.purs\""

-- Integration Testing API

sendCommand :: Value -> IO Text
sendCommand v = toS <$> readCreateProcess
  ((shell "psc-ide-client") { std_out=CreatePipe
                            , std_err=CreatePipe
                            })
  (T.unpack (encodeT v))

quitServer :: IO ()
quitServer = do
  let quitCommand = object ["command" .= ("quit" :: Text)]
  _ <- try $ sendCommand quitCommand :: IO (Either SomeException Text)
  return ()

reset :: IO ()
reset = do
  let resetCommand = object ["command" .= ("reset" :: Text)]
  _ <- try $ sendCommand resetCommand :: IO (Either SomeException Text)
  return ()

getCwd :: IO Text
getCwd = do
  let cwdCommand = object ["command" .= ("cwd" :: Text)]
  sendCommand cwdCommand

loadModule :: Text -> IO Text
loadModule m = loadModules [m]

loadModules :: [Text] -> IO Text
loadModules = sendCommand . load

loadAll :: IO Text
loadAll = sendCommand (load [])

getFlexCompletions :: Text -> IO [(Text, Text, Text, Maybe P.SourceSpan)]
getFlexCompletions q = parseCompletions <$> sendCommand (completion [] (Just (flexMatcher q)) Nothing)

getFlexCompletionsInModule :: Text -> Text -> IO [(Text, Text, Text, Maybe P.SourceSpan)]
getFlexCompletionsInModule q m = parseCompletions <$> sendCommand (completion [] (Just (flexMatcher q)) (Just m))

getType :: Text -> IO [(Text, Text, Text, Maybe P.SourceSpan)]
getType q = parseCompletions <$> sendCommand (typeC q [])

addImport :: Text -> FilePath -> FilePath -> IO Text
addImport identifier fp outfp = sendCommand (addImportC identifier fp outfp)

addImplicitImport :: Text -> FilePath -> FilePath -> IO Text
addImplicitImport mn fp outfp = sendCommand (addImplicitImportC mn fp outfp)

rebuildModule :: FilePath -> IO Text
rebuildModule m = sendCommand (rebuildC m Nothing)

-- Command Encoding

commandWrapper :: Text -> Value -> Value
commandWrapper c p = object ["command" .= c, "params" .= p]

load :: [Text] -> Value
load ms = commandWrapper "load" (object ["modules" .= ms])

typeC :: Text -> [Value] -> Value
typeC q filters = commandWrapper "type" (object ["search" .= q, "filters" .= filters])

addImportC :: Text -> FilePath -> FilePath -> Value
addImportC identifier = addImportW $
  object [ "importCommand" .= ("addImport" :: Text)
         , "identifier" .= identifier
         ]

addImplicitImportC :: Text -> FilePath -> FilePath -> Value
addImplicitImportC mn = addImportW $
  object [ "importCommand" .= ("addImplicitImport" :: Text)
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


completion :: [Value] -> Maybe Value -> Maybe Text -> Value
completion filters matcher currentModule =
  let
    matcher' = case matcher of
      Nothing -> []
      Just m -> ["matcher" .= m]
    currentModule' = case currentModule of
      Nothing -> []
      Just cm -> ["currentModule" .= cm]
  in
    commandWrapper "complete" (object $ "filters" .= filters : matcher' ++ currentModule' )

flexMatcher :: Text -> Value
flexMatcher q = object [ "matcher" .= ("flex" :: Text)
                       , "params" .= object ["search" .= q]
                       ]

-- Result parsing

unwrapResult :: Value -> Parser (Either Text Value)
unwrapResult = withObject "result" $ \o -> do
  (rt :: Text) <- o .: "resultType"
  case rt of
    "error" -> do
      res <- o .: "result"
      pure (Left res)
    "success" -> do
      res <- o .: "result"
      pure (Right res)
    _ -> mzero

withResult :: (Value -> Parser a) -> Value -> Parser (Either Text a)
withResult p v = do
  r <- unwrapResult v
  case r of
    Left err -> pure (Left err)
    Right res -> Right <$> p res

completionParser :: Value -> Parser [(Text, Text, Text, Maybe P.SourceSpan)]
completionParser = withArray "res" $ \cs ->
  mapM (withObject "completion" $ \o -> do
           ident <- o .: "identifier"
           module' <- o .: "module"
           ty <- o .: "type"
           ss <- o .: "definedAt"
           pure (module', ident, ty, ss)) (V.toList cs)

valueFromText :: Text -> Value
valueFromText = fromJust . decode . toS

resultIsSuccess :: Text -> Bool
resultIsSuccess = isRight . join . first toS . parseEither unwrapResult . valueFromText

parseCompletions :: Text -> [(Text, Text, Text, Maybe P.SourceSpan)]
parseCompletions s =
  fromJust $ join (rightToMaybe <$> parseMaybe (withResult completionParser) (valueFromText s))

parseTextResult :: Text -> Text
parseTextResult s =
  fromJust $ join (rightToMaybe <$> parseMaybe (withResult (withText "tr" pure)) (valueFromText s))
