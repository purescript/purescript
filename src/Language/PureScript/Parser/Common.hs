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

import Language.PureScript.Parser.Lexer
import Language.PureScript.Names

import qualified Text.Parsec as P

properName :: TokenParser u ProperName
properName = ProperName <$> uname

-- |
-- Parse a module name
--
moduleName :: TokenParser u ModuleName
moduleName = ModuleName <$> P.try (P.sepBy properName dot)

-- |
-- Parse a qualified name, i.e. M.name or just name
--
parseQualified :: TokenParser u a -> TokenParser u (Qualified a)
parseQualified parser = part []
  where
  part path = (do name <- P.try (properName <* delimiter)
                  part (updatePath path name))
              <|> (Qualified (qual path) <$> P.try parser)
  delimiter = dot <* P.notFollowedBy dot
  updatePath path name = path ++ [name]
  qual path = if null path then Nothing else Just $ ModuleName path

-- |
-- Parse an identifier or parenthesized operator
--
parseIdent :: TokenParser u Ident
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
-- Parse an identifier in backticks or an operator
--
parseIdentInfix :: TokenParser u (Qualified Ident)
parseIdentInfix = P.between tick tick (parseQualified (Ident <$> identifier)) <|> (parseQualified (Op <$> symbol))

-- |
-- Run a parser
--
runTokenParser :: FilePath -> TokenParser () a -> [PositionedToken] -> Either P.ParseError a
runTokenParser filePath p = P.parse p filePath

