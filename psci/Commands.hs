-----------------------------------------------------------------------------
--
-- Module      :  Commands
-- Copyright   :  (c) Phil Freeman 2014
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Commands for PSCI.
--
-----------------------------------------------------------------------------

module Commands where

import Control.Applicative
import System.Console.Haskeline

-- |
-- Valid Meta-commands for PSCI
--
data Command
  -- |
  -- And empty line
  --
  = Empty
  -- |
  -- A purescript expression
  --
  | Expression [String]
  -- |
  -- Show the help command
  --
  | Help
  -- |
  -- Import a module from a loaded file
  --
  | Import String
  -- |
  -- Load a file for use with importing
  --
  | LoadFile FilePath
  -- |
  -- Exit PSCI
  --
  | Quit
  -- |
  -- Reload the current modules
  --
  | Reload
  -- |
  -- Find the type of an expression
  --
  | TypeOf String
  -- |
  -- An attempt at a meta command that wasn't recognized.
  --
  | Unknown deriving (Show, Eq)

-- |
-- Parses the input and returns either a Metacommand or an expression.
--
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
    Just (':':'t':' ':expr) -> return $ TypeOf expr
    Just (':':_) -> return Unknown
    Just other -> Expression <$> go [other]
  where
  go ls = do
    l <- getInputLine "  "
    case l of
      Nothing -> return $ reverse ls
      Just l' -> go (l' : ls)

-- |
-- The help menu.
--
help :: [[String]]
help =
  [ [":?         ", "Show this help menu"]
  , [":i <module>", "Import <module> for use in PSCI"]
  , [":m <file>  ", "Load <file> for importing"]
  , [":q         ", "Quit PSCi"]
  , [":r         ", "Reload all modules."]
  ]
