module Commands where

import Control.Applicative
import System.Console.Haskeline

data Command
  = Empty
  | Expression [String]
  | Help
  | Import String
  | LoadFile FilePath
  | Quit
  | Reload
  | Unknown deriving (Show, Eq)

getCommand :: InputT IO Command
getCommand = do
  firstLine <- getInputLine "> "
  case firstLine of
    Nothing -> return Empty
    Just ":?" -> return Help
    Just (':':'i':' ':moduleName) -> return $ Import moduleName
    Just (':':'m':' ':filePath) -> return $ LoadFile filePath
    Just ":q" -> return Quit
    Just ":r" -> return Reload
    Just (':':_) -> return Unknown
    Just other -> Expression <$> go [other]
  where
  go ls = do
    l <- getInputLine "  "
    case l of
      Nothing -> return $ reverse ls
      Just l' -> go (l' : ls)

help :: [[String]]
help =
  [ [":?         ", "Show this help menu"]
  , [":i <module>", "Import <module> for use in PSCI"]
  , [":m <file>  ", "Load <file> for importing"]
  , [":q         ", "Quit PSCi"]
  , [":r         ", "Reload all modules."]
  ]
