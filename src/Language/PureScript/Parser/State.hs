-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Parser.State
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- State for the parser monad
--
-----------------------------------------------------------------------------

module Language.PureScript.Parser.State where

import qualified Text.Parsec as P

-- |
-- State for the parser monad
--
data ParseState = ParseState {
    -- |
    -- The most recently marked indentation level
    --
    indentationStack :: [P.Column]
  } deriving Show

pushIndentation :: P.Column -> ParseState -> ParseState
pushIndentation col (ParseState st) = ParseState (col : st)

popIndentation :: ParseState -> ParseState
popIndentation (ParseState []) = error "Attempt to pop empty stack"
popIndentation (ParseState (_ : st)) = ParseState st

popIndentationWhile :: (P.Column -> Bool) -> ParseState -> ParseState
popIndentationWhile p = ParseState . go . indentationStack
  where
  go [] = error "Attempt to pop empty stack"
  go (s : st) | p s = go st
              | otherwise = (s : st)

peekIndentation :: ParseState -> Maybe P.Column
peekIndentation (ParseState []) = Nothing
peekIndentation (ParseState (col : _)) = Just col