-- |
-- Directives for PSCI.
--
module Language.PureScript.Interactive.Directive where

import Prelude.Compat

import Data.Maybe (fromJust, listToMaybe)
import Data.List (isPrefixOf)
import Data.Tuple (swap)

import Language.PureScript.Interactive.Types

-- |
-- List of all avaliable directives.
--
directives :: [Directive]
directives = map fst directiveStrings

-- |
-- A mapping of directives to the different strings that can be used to invoke
-- them.
--
directiveStrings :: [(Directive, [String])]
directiveStrings =
    [ (Help   , ["?", "help"])
    , (Quit   , ["quit"])
    , (Reset  , ["reset"])
    , (Browse , ["browse"])
    , (Type   , ["type"])
    , (Kind   , ["kind"])
    , (Show   , ["show"])
    , (Paste  , ["paste"])
    ]

-- |
-- Like directiveStrings, but the other way around.
--
directiveStrings' :: [(String, Directive)]
directiveStrings' = concatMap go directiveStrings
  where
  go (dir, strs) = map (\s -> (s, dir)) strs

-- |
-- List of all directive strings.
--
strings :: [String]
strings = concatMap snd directiveStrings

-- |
-- Returns all possible string representations of a directive.
--
stringsFor :: Directive -> [String]
stringsFor d = fromJust (lookup d directiveStrings)

-- |
-- Returns the default string representation of a directive.
--
stringFor :: Directive -> String
stringFor = head . stringsFor

-- |
-- Returns the list of directives which could be expanded from the string
-- argument, together with the string alias that matched.
--
directivesFor' :: String -> [(Directive, String)]
directivesFor' str = go directiveStrings'
  where
  go = map swap . filter ((str `isPrefixOf`) . fst)

directivesFor :: String -> [Directive]
directivesFor = map fst . directivesFor'

directiveStringsFor :: String -> [String]
directiveStringsFor = map snd . directivesFor'

parseDirective :: String -> Maybe Directive
parseDirective = listToMaybe . directivesFor

-- |
-- True if the given directive takes an argument, false otherwise.
hasArgument :: Directive -> Bool
hasArgument Help = False
hasArgument Quit = False
hasArgument Reset = False
hasArgument Paste = False
hasArgument _ = True

-- |
-- The help menu.
--
help :: [(Directive, String, String)]
help =
  [ (Help,    "",         "Show this help menu")
  , (Quit,    "",         "Quit PSCi")
  , (Reset,   "",         "Discard all imported modules and declared bindings")
  , (Browse,  "<module>", "See all functions in <module>")
  , (Type,    "<expr>",   "Show the type of <expr>")
  , (Kind,    "<type>",   "Show the kind of <type>")
  , (Show,    "import",   "Show all imported modules")
  , (Show,    "loaded",   "Show all loaded modules")
  , (Paste,   "paste",    "Enter multiple lines, terminated by ^D")
  ]
