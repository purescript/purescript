-- |
-- Directives for PSCI.
--
module Language.PureScript.Interactive.Directive where

import Prelude

import Data.Maybe (fromJust)
import Data.List (isPrefixOf)
import Data.Tuple (swap)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NEL

import Language.PureScript.Interactive.Types (Directive(..))

-- |
-- A mapping of directives to the different strings that can be used to invoke
-- them.
--
directiveStrings :: [(Directive, NonEmpty String)]
directiveStrings =
    [ (Help      , NEL.fromList ["?", "help"])
    , (Quit      , NEL.singleton "quit")
    , (Reload    , NEL.singleton "reload")
    , (Clear     , NEL.singleton "clear")
    , (Browse    , NEL.singleton "browse")
    , (Type      , NEL.singleton "type")
    , (Kind      , NEL.singleton "kind")
    , (Show      , NEL.singleton "show")
    , (Paste     , NEL.singleton "paste")
    , (Complete  , NEL.singleton "complete")
    , (Print     , NEL.singleton "print")
    ]

-- |
-- Like `directiveStrings`, but the other way around.
--
directiveStrings' :: [(String, Directive)]
directiveStrings' = concatMap go directiveStrings
  where
  go (dir, strs) = map (, dir) $ NEL.toList strs

-- |
-- Returns all possible string representations of a directive.
--
stringsFor :: Directive -> NonEmpty String
stringsFor d = fromJust (lookup d directiveStrings)

-- |
-- Returns the default string representation of a directive.
--
stringFor :: Directive -> String
stringFor = NEL.head . stringsFor

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

-- |
-- The help menu.
--
help :: [(Directive, String, String)]
help =
  [ (Help,     "",          "Show this help menu")
  , (Quit,     "",          "Quit PSCi")
  , (Reload,   "",          "Reload all imported modules while discarding bindings")
  , (Clear,    "",          "Discard all imported modules and declared bindings")
  , (Browse,   "<module>",  "See all functions in <module>")
  , (Type,     "<expr>",    "Show the type of <expr>")
  , (Kind,     "<type>",    "Show the kind of <type>")
  , (Show,     "import",    "Show all imported modules")
  , (Show,     "loaded",    "Show all loaded modules")
  , (Show,     "print",     "Show the repl's current printing function")
  , (Paste,    "paste",     "Enter multiple lines, terminated by ^D")
  , (Complete, "<prefix>",  "Show completions for <prefix> as if pressing tab")
  , (Print,    "<fn>",      "Set the repl's printing function to <fn> (which must be fully qualified)")
  ]
