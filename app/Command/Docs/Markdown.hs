{-# LANGUAGE OverloadedStrings #-}

module Command.Docs.Markdown
  ( asMarkdown
  , writeMarkdownModules
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D
import qualified Language.PureScript.Docs.AsMarkdown as D
import           System.IO.UTF8 (writeUTF8FileT)

asMarkdown :: D.Module -> (P.ModuleName, Text)
asMarkdown m = (D.modName m, D.runDocs . D.moduleAsMarkdown $ m)

writeMarkdownModules :: FilePath -> [(P.ModuleName, Text)] -> IO ()
writeMarkdownModules outputDir = mapM_ $ writeMarkdownModule outputDir

writeMarkdownModule :: FilePath -> (P.ModuleName, Text) -> IO ()
writeMarkdownModule outputDir (mn, text) = do
  let filepath = outputDir ++ "/" ++ T.unpack (P.runModuleName mn) ++ ".md"
  writeUTF8FileT filepath text
