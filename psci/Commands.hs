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

import Language.PureScript

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
  | Expression Value
  -- |
  -- Show the help command
  --
  | Help
  -- |
  -- Import a module from a loaded file
  --
  | Import ModuleName
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
  -- Binds a value to a name
  --
  | Let (Value -> Value)
  -- |
  -- Find the type of an expression
  --
  | TypeOf Value

-- |
-- The help menu.
--
help :: [[String]]
help =
  [ [":?          ", "Show this help menu"]
  , [":i <module> ", "Import <module> for use in PSCI"]
  , [":m <file>   ", "Load <file> for importing"]
  , [":q          ", "Quit PSCi"]
  , [":r          ", "Reload all modules"]
  , [":t <expr>   ", "Show the type of <expr>"]
  ]
