----------------------------------------------------------------------------
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
import Control.Arrow (first)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)

import Options.Applicative

import Language.PureScript (parseModulesFromFiles)
import qualified Paths_purescript as Paths
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)
import Markdown
import Etags
import Ctags

-- Available output formats
data Format = Markdown -- Output documentation in Markdown format
               | Ctags -- Output ctags symbol index suitable for use with vi
               | Etags -- Output etags symbol index suitable for use with emacs

data PSCDocsOptions = PSCDocsOptions
  { pscdFormat :: Format
  , pscdInputFiles  :: [FilePath]
  }

docgen :: PSCDocsOptions -> IO ()
docgen (PSCDocsOptions fmt input) = do
  e <- parseModulesFromFiles (fromMaybe "") <$> mapM (fmap (first Just) . parseFile) (nub input)
  case e of
    Left err -> do
      hPutStrLn stderr $ show err
      exitFailure
    Right ms -> do
      case fmt of
       Markdown -> dumpMarkdown $ map snd ms
       Etags -> ldump $ dumpEtags $ pairs ms
       Ctags -> ldump $ dumpCtags $ pairs ms
      exitSuccess
    where pairs :: [(Maybe String, m)] -> [(String, m)]
          pairs = map (\(k,m) -> (fromMaybe "" k,m))
          ldump :: [String] -> IO ()
          ldump = mapM_ putStrLn


parseFile :: FilePath -> IO (FilePath, String)
parseFile input = (,) input <$> readFile input

inputFile :: Parser FilePath
inputFile = strArgument $
     metavar "FILE"
  <> help "The input .purs file(s)"

instance Read Format where
    readsPrec _ "etags" = [(Etags, "")]
    readsPrec _ "ctags" = [(Ctags, "")]
    readsPrec _ "markdown" = [(Markdown, "")]
    readsPrec _ _ = []    

format :: Parser Format
format = option auto $ value Markdown
         <> long "format"
         <> metavar "FORMAT"
         <> help "Set output FORMAT (markdown | etags | ctags)"

pscDocsOptions :: Parser PSCDocsOptions
pscDocsOptions = PSCDocsOptions <$> format
                                <*> many inputFile

main :: IO ()
main = execParser opts >>= docgen
  where
  opts        = info (version <*> helper <*> pscDocsOptions) infoModList
  infoModList = fullDesc <> headerInfo <> footerInfo
  headerInfo  = header   "psc-docs - Generate Markdown documentation from PureScript extern files"
  footerInfo  = footer $ "psc-docs " ++ showVersion Paths.version

  version :: Parser (a -> a)
  version = abortOption (InfoMsg (showVersion Paths.version)) $ long "version" <> help "Show the version number" <> hidden
