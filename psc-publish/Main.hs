
module Main where

import Data.Version (showVersion)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL

import Options.Applicative hiding (str)

import qualified Paths_purescript as Paths
import Language.PureScript.Publish (preparePackage)

main :: IO ()
main = execParser opts >> publish
  where
  opts        = info (version <*> helper) infoModList
  infoModList = fullDesc <> headerInfo <> footerInfo
  headerInfo  = header "psc-publish - Generates documentation packages for upload to http://pursuit.purescript.org"
  footerInfo  = footer $ "psc-publish " ++ showVersion Paths.version

  version :: Parser (a -> a)
  version = abortOption (InfoMsg (showVersion Paths.version)) $ long "version" <> help "Show the version number" <> hidden

publish :: IO ()
publish = do
  pkg <- preparePackage
  BL.putStrLn (A.encode pkg)

