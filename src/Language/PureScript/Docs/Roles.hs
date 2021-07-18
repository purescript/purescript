-- | Various constants when referring to roles and their external documentation
module Language.PureScript.Docs.Roles where

import Data.Text (Text)

-- |
-- URL to the documentation repo's "Roles" page
docRepoRolesPage :: Text
docRepoRolesPage = "https://github.com/purescript/documentation/blob/master/language/Roles.md"

-- |
-- Tooltip text that appears in docs when one hovers over the `nominal` role.
describeNominal :: Text
describeNominal =
  "The 'nominal' role means this argument may not change when coercing the type."

-- |
-- Tooltip text that appears in docs when one hovers over the `phantom` role.
describePhantom :: Text
describePhantom =
  "The 'phantom' role means this argument can change freely when coercing the type."
