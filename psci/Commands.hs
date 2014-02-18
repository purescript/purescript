module Commands where

import Control.Applicative
import System.Console.Haskeline

data Command
  = Empty
  | Expression [String]
  | Import String
  | Let String
  | LoadModule FilePath
  | Reload deriving (Show, Eq)

getCommand :: InputT IO Command
getCommand = do
  firstLine <- getInputLine  "> "
  case firstLine of
    Nothing -> return Empty
    Just (':':'i':' ':moduleName) -> return $ Import moduleName
    Just (':':'m':' ':modulePath) -> return $ LoadModule modulePath
    Just ":r" -> return Reload
    Just (':':_) -> outputStrLn "Unknown command" >> getCommand
    Just l@('l':'e':'t':_) -> return $ Let l
    Just other -> Expression <$> go [other]
  where
  go ls = do
    l <- getInputLine "  "
    case l of
      Nothing -> return $ reverse ls
      Just l' -> go (l' : ls)
