-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Parser.Common
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Constants, and utility functions to be used when parsing
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Language.PureScript.Parser.Common where

import Control.Applicative
import Control.Monad (guard)

import Language.PureScript.Comments
import Language.PureScript.Parser.Lexer
import Language.PureScript.Parser.State
import Language.PureScript.Names

import qualified Text.Parsec as P

featureWasRemoved :: String -> TokenParser a
featureWasRemoved err = do
  pos <- P.getPosition
  error $ "It looks like you are trying to use a feature from a previous version of the compiler:\n" ++ err ++ "\nat " ++ show pos

properName :: TokenParser ProperName
properName = ProperName <$> uname

-- |
-- Parse a module name
--
moduleName :: TokenParser ModuleName
moduleName = part []
  where
  part path = (do name <- ProperName <$> P.try qualifier
                  part (path `snoc` name))
              <|> (ModuleName . snoc path . ProperName <$> mname)
  snoc path name = path ++ [name]

-- |
-- Parse a qualified name, i.e. M.name or just name
--
parseQualified :: TokenParser a -> TokenParser (Qualified a)
parseQualified parser = part []
  where
  part path = (do name <- ProperName <$> P.try qualifier
                  part (updatePath path name))
              <|> (Qualified (qual path) <$> P.try parser)
  updatePath path name = path ++ [name]
  qual path = if null path then Nothing else Just $ ModuleName path

-- |
-- Parse an identifier or parenthesized operator
--
parseIdent :: TokenParser Ident
parseIdent = (Ident <$> identifier) <|> (Op <$> parens symbol)

-- |
-- Run the first parser, then match the second if possible, applying the specified function on a successful match
--
augment :: P.Stream s m t => P.ParsecT s u m a -> P.ParsecT s u m b -> (a -> b -> a) -> P.ParsecT s u m a
augment p q f = flip (maybe id $ flip f) <$> p <*> P.optionMaybe q

-- |
-- Run the first parser, then match the second zero or more times, applying the specified function for each match
--
fold :: P.Stream s m t => P.ParsecT s u m a -> P.ParsecT s u m b -> (a -> b -> a) -> P.ParsecT s u m a
fold first more combine = do
  a <- first
  bs <- P.many more
  return $ foldl combine a bs

-- |
-- Build a parser from a smaller parser and a list of parsers for postfix operators
--
buildPostfixParser :: P.Stream s m t => [a -> P.ParsecT s u m a] -> P.ParsecT s u m a -> P.ParsecT s u m a
buildPostfixParser fs first = do
  a <- first
  go a
  where
  go a = do
    maybeA <- P.optionMaybe $ P.choice (map ($ a) fs)
    case maybeA of
      Nothing -> return a
      Just a' -> go a'

-- |
-- Mark the current indentation level
--
mark :: P.Parsec s ParseState a -> P.Parsec s ParseState a
mark p = do
  current <- indentationLevel <$> P.getState
  pos <- P.sourceColumn <$> P.getPosition
  P.modifyState $ \st -> st { indentationLevel = pos }
  a <- p
  P.modifyState $ \st -> st { indentationLevel = current }
  return a

-- |
-- Check that the current identation level matches a predicate
--
checkIndentation :: (P.Column -> P.Column -> Bool) -> P.Parsec s ParseState ()
checkIndentation rel = do
  col <- P.sourceColumn <$> P.getPosition
  current <- indentationLevel <$> P.getState
  guard (col `rel` current)

-- |
-- Check that the current indentation level is past the current mark
--
indented :: P.Parsec s ParseState ()
indented = checkIndentation (>) P.<?> "indentation"

-- |
-- Check that the current indentation level is at the same indentation as the current mark
--
same :: P.Parsec s ParseState ()
same = checkIndentation (==) P.<?> "no indentation"

-- |
-- Read the comments from the the next token, without consuming it
--
readComments :: P.Parsec [PositionedToken] u [Comment]
readComments = P.lookAhead $ ptComments <$> P.anyToken

-- |
-- Run a parser
--
runTokenParser :: FilePath -> TokenParser a -> [PositionedToken] -> Either P.ParseError a
runTokenParser filePath p = P.runParser p (ParseState 0) filePath
