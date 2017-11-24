-- |
-- Directives for PSCI.
--
module Language.PureScript.Interactive.Directive where

import PSPrelude

import Data.List (lookup)
import qualified Data.Text as T

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
directiveStrings :: [(Directive, [Text])]
directiveStrings =
    [ (Help      , ["?", "help"])
    , (Quit      , ["quit"])
    , (Reload    , ["reload"])
    , (Clear     , ["clear"])
    , (Browse    , ["browse"])
    , (Type      , ["type"])
    , (Kind      , ["kind"])
    , (Show      , ["show"])
    , (Paste     , ["paste"])
    , (Complete  , ["complete"])
    ]

-- |
-- Like directiveStrings, but the other way around.
--
directiveStrings' :: [(Text, Directive)]
directiveStrings' = concatMap go directiveStrings
  where
  go (dir, strs) = map (\s -> (s, dir)) strs

-- |
-- List of all directive strings.
--
strings :: [Text]
strings = concatMap snd directiveStrings

-- |
-- Returns all possible string representations of a directive.
--
stringsFor :: Directive -> [Text]
stringsFor d = unsafeFromJust (lookup d directiveStrings)

-- |
-- Returns the default string representation of a directive.
--
stringFor :: Directive -> Text
stringFor = unsafeHead . stringsFor

-- |
-- Returns the list of directives which could be expanded from the string
-- argument, together with the string alias that matched.
--
directivesFor' :: Text -> [(Directive, Text)]
directivesFor' str = go directiveStrings'
  where
  go = map swap . filter ((str `T.isPrefixOf`) . fst)

directivesFor :: Text -> [Directive]
directivesFor = map fst . directivesFor'

directiveStringsFor :: Text -> [Text]
directiveStringsFor = map snd . directivesFor'

parseDirective :: Text -> Maybe Directive
parseDirective = listToMaybe . directivesFor

-- |
-- True if the given directive takes an argument, false otherwise.
hasArgument :: Directive -> Bool
hasArgument Help = False
hasArgument Quit = False
hasArgument Reload = False
hasArgument Clear = False
hasArgument Paste = False
hasArgument _ = True

-- |
-- The help menu.
--
help :: [(Directive, Text, Text)]
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
  , (Paste,    "paste",     "Enter multiple lines, terminated by ^D")
  , (Complete, "<prefix>",  "Show completions for <prefix> as if pressing tab")
  ]

