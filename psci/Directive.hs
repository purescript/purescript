-----------------------------------------------------------------------------
--
-- Module      :  Directive
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :  experimental
-- Portability :
--
-- |
-- Directives for PSCI.
--
-----------------------------------------------------------------------------

module Directive where

import Data.List (nub, isPrefixOf)

data Directive
  = Help
  | Quit
  | Reset
  | Browse
  | Load
  | Type
  | Kind
  | Show
  deriving Eq

-- |
-- Maps given directive to relating command strings.
--
commands :: Directive -> [String]
commands Help = ["?", "help"]
commands Quit = ["quit"]
commands Reset = ["reset"]
commands Browse = ["browse"]
commands Load = ["load", "module"]
commands Type = ["type"]
commands Kind = ["kind"]
commands Show = ["show"]

-- |
-- Tries to parse given string into a directive.
--
parseDirective :: String -> Maybe Directive
parseDirective cmd =
  case filter (matches . snd) mapping of
    [directive] -> Just $ fst directive
    _ -> Nothing
  where
  mapping :: [(Directive, [String])]
  mapping = zip directives (map commands directives)

  matches :: [String] -> Bool
  matches = any (cmd `isPrefixOf`)

-- |
-- The help menu.
--
help :: [(Directive, String, String)]
help =
  [ (Help,   "",         "Show this help menu")
  , (Quit,   "",         "Quit PSCi")
  , (Reset,  "",         "Reset")
  , (Browse, "<module>", "Browse <module>")
  , (Load,   "<file>",   "Load <file> for importing")
  , (Type,   "<expr>",   "Show the type of <expr>")
  , (Kind,   "<type>",   "Show the kind of <type>")
  , (Show,   "import",   "Show imported modules")
  , (Show,   "loaded",   "Show loaded modules")
  ]

-- |
-- List of all avaliable directives.
--
directives :: [Directive]
directives = nub . map (\(dir, _, _) -> dir) $ help
