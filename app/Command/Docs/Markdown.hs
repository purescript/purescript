module Command.Docs.Markdown
  ( asMarkdown
  , writeMarkdownModules
  ) where

import Prelude

import Data.Text (Text)
import Data.Text qualified as T
import Language.PureScript.Names qualified as PN
import Language.PureScript.Docs.Types ( Module(modName) )
import Language.PureScript.Docs.AsMarkdown qualified as DMark
import System.IO.UTF8 (writeUTF8FileT)

asMarkdown :: Module -> (PN.ModuleName, Text)
asMarkdown m = (modName m, DMark.runDocs . DMark.moduleAsMarkdown $ m)

writeMarkdownModules :: FilePath -> [(PN.ModuleName, Text)] -> IO ()
writeMarkdownModules outputDir = mapM_ $ writeMarkdownModule outputDir

writeMarkdownModule :: FilePath -> (PN.ModuleName, Text) -> IO ()
writeMarkdownModule outputDir (mn, text) = do
  let filepath = outputDir ++ "/" ++ T.unpack (PN.runModuleName mn) ++ ".md"
  writeUTF8FileT filepath text
