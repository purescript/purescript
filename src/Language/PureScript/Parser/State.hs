-- |
-- State for the parser monad
--
module Language.PureScript.Parser.State where

import Prelude.Compat

import qualified Text.Parsec as P

-- |
-- State for the parser monad
--
data ParseState = ParseState {
    -- |
    -- The most recently marked indentation level
    --
    indentationLevel :: P.Column
  } deriving Show
