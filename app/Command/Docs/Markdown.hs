module Command.Docs.Markdown
  ( asMarkdown
  , writeMarkdownModules
  ) where

import Prelude

import Data.Text (Text)
import Data.Text qualified as T
import Language.PureScript qualified as P
import Language.PureScript.Docs qualified as D
import Language.PureScript.Docs.AsMarkdown qualified as D
import System.IO.UTF8 (writeUTF8FileT)

asMarkdown :: D.Module -> (P.ModuleName, Text)
asMarkdown m = (D.modName m, D.runDocs . D.moduleAsMarkdown $ m)

writeMarkdownModules :: FilePath -> [(P.ModuleName, Text)] -> IO ()
writeMarkdownModules outputDir = mapM_ $ writeMarkdownModule outputDir

writeMarkdownModule :: FilePath -> (P.ModuleName, Text) -> IO ()
writeMarkdownModule outputDir (mn, text) = do
  let filepath = outputDir ++ "/" ++ T.unpack (P.runModuleName mn) ++ ".md"
  writeUTF8FileT filepath text
