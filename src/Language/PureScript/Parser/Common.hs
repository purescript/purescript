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

import Data.Functor.Identity
import Control.Applicative
import Control.Monad
import Language.PureScript.Parser.State
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as PT

import Language.PureScript.Names

-- |
-- A list of purescript reserved identifiers
--
reservedPsNames :: [String]
reservedPsNames = [ "data"
                  , "type"
                  , "foreign"
                  , "import"
                  , "infixl"
                  , "infixr"
                  , "class"
                  , "instance"
                  , "module"
                  , "case"
                  , "of"
                  , "if"
                  , "then"
                  , "else"
                  , "do"
                  , "let"
                  , "true"
                  , "false"
                  , "in"
                  , "where"
                  ]

-- |
-- A list of reserved identifiers for types
--
reservedTypeNames :: [String]
reservedTypeNames = [ "forall"
                    , "where" ]

-- |
-- A list of reserved operators
--
reservedOpNames :: [String]
reservedOpNames = [ "=>", "->", "=", ".", "\\" ]

-- |
-- Valid first characters for an identifier
--
identStart :: P.Parsec String u Char
identStart = P.lower <|> P.oneOf "_"

-- |
-- Valid identifier characters
--
identLetter :: P.Parsec String u Char
identLetter = P.alphaNum <|> P.oneOf "_'"

-- |
-- Valid first characters for an operator
--
opStart :: P.Parsec String u Char
opStart = P.oneOf ":!#$%&*+./<=>?@\\^|-~"

-- |
-- Valid operators characters
--
opLetter :: P.Parsec String u Char
opLetter = P.oneOf ":!#$%&*+./<=>?@\\^|-~"

-- |
-- The PureScript language definition
--
langDef :: PT.GenLanguageDef String u Identity
langDef = PT.LanguageDef
  { PT.reservedNames   = reservedPsNames
  , PT.reservedOpNames = reservedOpNames
  , PT.commentStart    = "{-"
  , PT.commentEnd      = "-}"
  , PT.commentLine     = "--"
  , PT.nestedComments  = True
  , PT.identStart      = identStart
  , PT.identLetter     = identLetter
  , PT.opStart         = opStart
  , PT.opLetter        = opLetter
  , PT.caseSensitive   = True
  }

-- |
-- A token parser based on the language definition
--
tokenParser :: PT.GenTokenParser String u Identity
tokenParser = PT.makeTokenParser langDef

-- |
-- Parse a token
--
lexeme :: P.Parsec String u a -> P.Parsec String u a
lexeme = PT.lexeme tokenParser

-- |
-- Parse an identifier
--
identifier :: P.Parsec String u String
identifier = PT.identifier tokenParser

-- |
-- Parse a reserved word
--
reserved :: String -> P.Parsec String u ()
reserved = PT.reserved tokenParser

-- |
-- Parse a reserved operator
--
reservedOp :: String -> P.Parsec String u ()
reservedOp = PT.reservedOp tokenParser

-- |
-- Parse an operator
--
operator :: P.Parsec String u String
operator = PT.operator tokenParser

-- |
-- Parse a string literal
--
stringLiteral :: P.Parsec String u String
stringLiteral = PT.stringLiteral tokenParser

-- |
-- Parse whitespace
--
whiteSpace :: P.Parsec String u ()
whiteSpace = PT.whiteSpace tokenParser

-- |
-- Semicolon
--
semi :: P.Parsec String u String
semi = PT.semi tokenParser

-- |
-- Colon
--
colon :: P.Parsec String u String
colon = PT.colon tokenParser

-- |
-- Period
--
dot :: P.Parsec String u String
dot = PT.dot tokenParser

-- |
-- Comma
--
comma :: P.Parsec String u String
comma = PT.comma tokenParser

-- |
-- Backtick
--
tick :: P.Parsec String u Char
tick = lexeme $ P.char '`'

-- |
-- Pipe character
--
pipe :: P.Parsec String u Char
pipe = lexeme $ P.char '|'

-- |
-- Natural number
--
natural :: P.Parsec String u Integer
natural = PT.natural tokenParser

-- |
-- Parse a proper name
--
properName :: P.Parsec String u ProperName
properName = lexeme $ ProperName <$> P.try ((:) <$> P.upper <*> many P.alphaNum P.<?> "name")

-- |
-- Parse a module name
--
moduleName :: P.Parsec String ParseState ModuleName
moduleName = ModuleName <$> P.try (sepBy properName dot)

-- |
-- Parse a qualified name, i.e. M.name or just name
--
parseQualified :: P.Parsec String ParseState a -> P.Parsec String ParseState (Qualified a)
parseQualified parser = part []
  where
  part path = (do name <- P.try (properName <* delimiter)
                  part (updatePath path name))
              <|> (Qualified (qual path) <$> P.try parser)
  delimiter = indented *> dot <* P.notFollowedBy dot
  updatePath path name = path ++ [name]
  qual path = if null path then Nothing else Just $ ModuleName path

-- |
-- Parse an integer or floating point value
--
integerOrFloat :: P.Parsec String u (Either Integer Double)
integerOrFloat = (Right <$> P.try (PT.float tokenParser) <|>
                  Left <$> P.try (PT.natural tokenParser)) P.<?> "number"

-- |
-- Parse an identifier or parenthesized operator
--
parseIdent :: P.Parsec String ParseState Ident
parseIdent = (Ident <$> identifier) <|> (Op <$> parens operator)

-- |
-- Parse a token inside square brackets
--
squares :: P.Parsec String ParseState a -> P.Parsec String ParseState a
squares = P.between (lexeme $ P.char '[') (lexeme $ indented *> P.char ']') . (indented *>)

-- |
-- Parse a token inside parentheses
--
parens :: P.Parsec String ParseState a -> P.Parsec String ParseState a
parens = P.between (lexeme $ P.char '(') (lexeme $ indented *> P.char ')') . (indented *>)

-- |
-- Parse a token inside braces
--
braces :: P.Parsec String ParseState a -> P.Parsec String ParseState a
braces = P.between (lexeme $ P.char '{') (lexeme $ indented *> P.char '}') . (indented *>)

-- |
-- Parse a token inside angle brackets
--
angles :: P.Parsec String ParseState a -> P.Parsec String ParseState a
angles = P.between (lexeme $ P.char '<') (lexeme $ indented *> P.char '>') . (indented *>)

-- |
-- Parse zero or more values separated by a separator token
--
sepBy :: P.Parsec String ParseState a -> P.Parsec String ParseState sep -> P.Parsec String ParseState [a]
sepBy p s = P.sepBy (indented *> p) (indented *> s)

-- |
-- Parse one or more values separated by a separator token
--
sepBy1 :: P.Parsec String ParseState a -> P.Parsec String ParseState sep -> P.Parsec String ParseState [a]
sepBy1 p s = P.sepBy1 (indented *> p) (indented *> s)

-- |
-- Parse zero or more values separated by semicolons
--
semiSep :: P.Parsec String ParseState a -> P.Parsec String ParseState [a]
semiSep = flip sepBy semi

-- |
-- Parse one or more values separated by semicolons
--
semiSep1 :: P.Parsec String ParseState a -> P.Parsec String ParseState [a]
semiSep1 = flip sepBy1 semi

-- |
-- Parse zero or more values separated by commas
--
commaSep :: P.Parsec String ParseState a -> P.Parsec String ParseState [a]
commaSep = flip sepBy comma

-- |
-- Parse one or more values separated by commas
--
commaSep1 :: P.Parsec String ParseState a -> P.Parsec String ParseState [a]
commaSep1 = flip sepBy1 comma

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
parseIdentInfix :: P.Parsec String ParseState (Qualified Ident)
parseIdentInfix = P.between tick tick (parseQualified (Ident <$> identifier)) <|> (parseQualified (Op <$> operator))

-- |
-- Mark the current indentation level
--
mark :: P.Parsec String ParseState a -> P.Parsec String ParseState a
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
checkIndentation :: (P.Column -> P.Column -> Bool) -> P.Parsec String ParseState ()
checkIndentation rel = do
  col <- P.sourceColumn <$> P.getPosition
  current <- indentationLevel <$> P.getState
  guard (col `rel` current)

-- |
-- Check that the current indentation level is past the current mark
--
indented :: P.Parsec String ParseState ()
indented = checkIndentation (>) P.<?> "indentation"

-- |
-- Check that the current indentation level is at the same indentation as the current mark
--
same :: P.Parsec String ParseState ()
same = checkIndentation (==) P.<?> "no indentation"

-- |
-- Run a parser which supports indentation
--
runIndentParser :: FilePath -> P.Parsec String ParseState a -> String -> Either P.ParseError a
runIndentParser filePath p = P.runParser p (ParseState 0) filePath
