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
  -- A purescript expression
  --
  = Expression Expr
  -- |
  -- Show the help command
  --
  | Help
  -- |
  -- Import a module from a loaded file
  --
  | Import ModuleName
  -- |
  -- Browse a module
  --
  | Browse ModuleName
  -- |
  -- Load a file for use with importing
  --
  | LoadFile FilePath
  -- |
  -- Exit PSCI
  --
  | Quit
  -- |
  -- Reset the state of the REPL
  --
  | Reset
  -- |
  -- Binds a value to a name
  --
  | Let [Declaration]
  -- |
  -- Find the type of an expression
  --
  | TypeOf Expr
  -- |
  -- Find the kind of an expression
  --
  | KindOf Type
  -- |
  -- Show command
  --
  | Show String

-- |
-- The help menu.
--
help :: [(String, String, String)]
help =
  [ (":?", "",         "Show this help menu")
  , (":i", "<module>", "Import <module> for use in PSCI")
  , (":b", "<module>", "Browse <module>")
  , (":m", "<file>",   "Load <file> for importing")
  , (":q", "",         "Quit PSCi")
  , (":r", "",         "Reset")
  , (":s", "import",   "Show imported modules")
  , (":s", "loaded",   "Show loaded modules")
  , (":t", "<expr>",   "Show the type of <expr>")
  , (":k", "<type>",   "Show the kind of <type>")
  ]

commands :: [String]
commands = map (\ (a, _, _) -> a) help
