-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) Hardy Jones 2014
-- License     :  MIT
--
-- Maintainer  :  Hardy Jones <jones3.hardy@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Generate Directed Graphs of PureScript TypeClasses
--
-----------------------------------------------------------------------------

module Main where

import Control.Monad (unless)

import Data.List (intercalate,nub,sort)
import Data.Foldable (for_)
import Data.Version (showVersion)

import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)

import Text.Parsec as Par (ParseError)

import qualified Language.PureScript as P
import qualified Paths_purescript as Paths
import qualified System.IO.UTF8 as U


data HierarchyOptions = HierarchyOptions
  { hierachyInput   :: FilePath
  , hierarchyOutput :: Maybe FilePath
  }

newtype SuperMap = SuperMap { unSuperMap :: Either P.ProperName (P.ProperName, P.ProperName) }
  deriving Eq

instance Show SuperMap where
  show (SuperMap (Left sub)) = show sub
  show (SuperMap (Right (super, sub))) = show super ++ " -> " ++ show sub

instance Ord SuperMap where
  compare (SuperMap s) (SuperMap s') = getCls s `compare` getCls s'
    where
      getCls = either id snd

runModuleName :: P.ModuleName -> String
runModuleName (P.ModuleName pns) = intercalate "_" (P.runProperName `map` pns)

readInput :: FilePath -> IO (Either Par.ParseError [P.Module])
readInput filename = do
  content <- U.readFile filename
  return $ fmap (map snd) $ P.parseModulesFromFiles id [(filename, content)]

compile :: HierarchyOptions -> IO ()
compile (HierarchyOptions input mOutput) = do
  modules <- readInput input
  case modules of
    Left err -> U.hPutStr stderr (show err) >> exitFailure
    Right ms -> do
      for_ ms $ \(P.Module moduleName decls _) ->
        let name = runModuleName moduleName
            tcs = filter P.isTypeClassDeclaration decls
            supers = sort . nub . filter (not . null) $ fmap superClasses tcs
            prologue = "digraph " ++ name ++ " {\n"
            body = intercalate "\n" (concatMap (fmap (\s -> "  " ++ show s ++ ";")) supers)
            epilogue = "\n}"
            hier = prologue ++ body ++ epilogue
        in unless (null supers) $ case mOutput of
          Just output -> do
            createDirectoryIfMissing True output
            U.writeFile (output </> name) hier
          Nothing -> U.putStrLn hier
      exitSuccess

superClasses :: P.Declaration -> [SuperMap]
superClasses (P.TypeClassDeclaration sub _ supers@(_:_) _) =
  fmap (\(P.Qualified _ super, _) -> SuperMap (Right (super, sub))) supers
superClasses (P.TypeClassDeclaration sub _ _ _) = [SuperMap (Left sub)]
superClasses (P.PositionedDeclaration _ decl) = superClasses decl
superClasses _ = []

inputFile :: Parser FilePath
inputFile = strArgument $
     metavar "FILE"
  <> value "main.purs"
  <> showDefault
  <> help "The input file to generate a hierarchy from"

outputFile :: Parser (Maybe FilePath)
outputFile = optional . strOption $
     short 'o'
  <> long "output"
  <> help "The output directory"

pscOptions :: Parser HierarchyOptions
pscOptions = HierarchyOptions <$> inputFile
                              <*> outputFile

main :: IO ()
main = execParser opts >>= compile
  where
  opts        = info (helper <*> pscOptions) infoModList
  infoModList = fullDesc <> headerInfo <> footerInfo
  headerInfo  = header   "hierarchy - Creates a GraphViz directed graph of PureScript TypeClasses"
  footerInfo  = footer $ "hierarchy " ++ showVersion Paths.version

