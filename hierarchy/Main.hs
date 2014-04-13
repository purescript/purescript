-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) Phil Freeman 2014
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Generate Directed Graphs of PureScript TypeClasses
--
-----------------------------------------------------------------------------

module Main where

import Control.Applicative ((<*>), (<$>))

import Data.List (intercalate,nub,sort)
import Data.Version (showVersion)

import System.Console.CmdTheLine
import System.Exit (exitFailure, exitSuccess)

import Text.Parsec (ParseError)

import qualified Language.PureScript as P
import qualified Paths_purescript as Paths
import qualified System.IO.UTF8 as U

newtype SuperMap = SuperMap { unSuperMap :: Either P.ProperName (P.ProperName, P.ProperName) }
  deriving Eq

instance Show SuperMap where
  show (SuperMap (Left sub)) = show sub
  show (SuperMap (Right (super, sub))) = show super ++ " -> " ++ show sub

instance Ord SuperMap where
  compare (SuperMap s) (SuperMap s') = getCls s `compare` getCls s'
    where
      getCls = either id snd

readInput :: FilePath -> IO (Either ParseError [P.Module])
readInput p = do
  text <- U.readFile p
  return $ P.runIndentParser p P.parseModules text

compile :: FilePath -> Maybe FilePath -> IO ()
compile input mOutput = do
  modules <- readInput input
  case modules of
    Left err -> U.print err >> exitFailure
    Right (P.Module _ decls _ : _) -> do
      let tcs = filter P.isTypeClassDeclaration decls
      let supers = sort . nub . filter (not . null) $ fmap superClasses tcs
      let hier = "digraph Prelude {\n" ++ intercalate "\n" (concatMap (fmap (("  " ++) . (++ ";") . show)) supers) ++ "\n}"
      case mOutput of
        Just output -> U.writeFile output hier
        Nothing      -> U.putStrLn hier
      exitSuccess

superClasses :: P.Declaration -> [SuperMap]
superClasses (P.TypeClassDeclaration sub _ supers@(_:_) _) =
  fmap (\(P.Qualified _ super, _) -> SuperMap (Right (super, sub))) supers
superClasses (P.TypeClassDeclaration sub _ _ _) = [SuperMap (Left sub)]
superClasses (P.PositionedDeclaration _ decl) = superClasses decl
superClasses _ = []

outputFile :: Term (Maybe FilePath)
outputFile = value $ opt Nothing $ (optInfo [ "o", "output" ])
     { optDoc = "The output file" }

inputFile :: Term FilePath
inputFile = value $ pos 0 "main.purs" $ posInfo { posDoc = "The input file to generate a hierarchy from" }

term :: Term (IO ())
term = compile <$> inputFile <*> outputFile

termInfo :: TermInfo
termInfo = defTI
  { termName = "hierarchy"
  , version  = showVersion Paths.version
  , termDoc  = "Creates a GraphViz directed graph of PureScript TypeClasses"
  }

main :: IO ()
main = run (term, termInfo)
