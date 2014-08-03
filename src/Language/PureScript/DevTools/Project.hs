{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.DevTools.Project (filesForProject, findProjectFile) where

import System.Directory
import System.FilePath.Glob
import Data.List
import Data.List.Split
import Data.Aeson
import Control.Monad
import Data.Functor
import qualified Data.ByteString.Lazy as BS

-- | Representation of the JSON project file
data ProjectFile = ProjectFile {
    projectFiles :: [FilePath]
}

-- | Actual representation for common usages
data Project = Project {
    globs :: [Pattern],
    wd :: FilePath
} deriving (Show)

instance FromJSON ProjectFile where
	parseJSON (Object v) = ProjectFile <$> v .: "files"
	parseJSON _  = mzero

-- | Name of a project file
projectFileName :: String
projectFileName = "purs.json"

-- |
-- Turns a path into the corresponding project representation
-- Can throw error if the file is not a valid JSON representation
fromPathToProject :: FilePath -> IO Project
fromPathToProject p = do
    content <- BS.readFile p
    let minfos = decode content :: Maybe ProjectFile
    case minfos of
    	Nothing -> error "Found project file but can't read it"
    	Just infos -> return $ Project (map compile $ projectFiles infos) (intercalate "/" $ init $ splitOn "/" p)

-- |
-- Recursively search backward for the project file
--
inFolder :: [String] -> IO (Maybe Project)
inFolder [] = return Nothing
inFolder xs = do
    let p = intercalate "/" xs
    let filep = p ++ "/" ++ projectFileName
    exist <- doesFileExist filep
    if exist
        then do
            prj <- fromPathToProject filep
            return $ Just prj
        else inFolder (init xs)

-- |
-- Search for a project file in the file tree
--
findProjectFile :: FilePath -> IO (Maybe Project)
findProjectFile path = inFolder folders
    where folders = splitOn "/" path


-- |
-- Returns all files matching globs in the Project representation
--
matchingFiles :: Project -> IO [FilePath]
matchingFiles prj = do
    files <- globDir patterns (wd prj)
    return $ listOfFiles files
    where patterns = globs prj
          listOfFiles :: ([[String]], [String]) -> [String]
          listOfFiles = nub . concat. fst

-- |
-- Shortcut function that turns the current path into files included in current project
filesForProject :: FilePath -> IO [FilePath]
filesForProject path = do
    mprj <- findProjectFile path
    case mprj of
        Nothing -> do
            putStrLn "*** No project file found ***"
            return []
        Just prj -> matchingFiles prj
