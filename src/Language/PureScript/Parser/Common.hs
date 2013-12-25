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

reservedNames :: [String]
reservedNames = [ "case"
                , "of"
                , "data"
                , "type"
                , "var"
                , "val"
                , "while"
                , "for"
                , "foreach"
                , "if"
                , "then"
                , "else"
                , "return"
                , "true"
                , "false"
                , "foreign"
                , "import"
                , "member"
                , "forall"
                , "do"
                , "until"
                , "in"
                , "break"
                , "catch"
                , "continue"
                , "debugger"
                , "default"
                , "delete"
                , "finally"
                , "function"
                , "instanceof"
                , "new"
                , "switch"
                , "this"
                , "throw"
                , "try"
                , "typeof"
                , "void"
                , "with"
                , "Number"
                , "String"
                , "Boolean"
                , "infixl"
                , "infixr"
                , "module" ]

builtInOperators :: [String]
builtInOperators = [ "~", "-", "<=", ">=", "<", ">", "*", "/", "%", "++", "+", "<<", ">>>", ">>"
                  , "==", "!=", "&&", "||", "&", "^", "|", "!!", "!" ]

reservedOpNames :: [String]
reservedOpNames = builtInOperators ++ [ "->", "=", "." ]

identStart :: P.Parsec String u Char
identStart = P.lower <|> P.oneOf "_$"

properNameStart :: P.Parsec String u Char
properNameStart = P.upper

identLetter :: P.Parsec String u Char
identLetter = P.alphaNum <|> P.oneOf "_'"

opStart :: P.Parsec String u Char
opStart = P.oneOf ":.!#%&*+/<=>?@^|~"

opLetter :: P.Parsec String u Char
opLetter = P.oneOf ":.#$%&*+./<=>?@^|"

langDef :: PT.GenLanguageDef String u Identity
langDef = PT.LanguageDef
  { PT.reservedNames   = reservedNames
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

tokenParser :: PT.GenTokenParser String u Identity
tokenParser = PT.makeTokenParser langDef

lexeme :: P.Parsec String u a -> P.Parsec String u a
lexeme = PT.lexeme tokenParser

identifier :: P.Parsec String u String
identifier = PT.identifier tokenParser

reserved :: String -> P.Parsec String u ()
reserved = PT.reserved tokenParser

reservedOp :: String -> P.Parsec String u ()
reservedOp = PT.reservedOp tokenParser

operator :: P.Parsec String u String
operator = PT.operator tokenParser

stringLiteral :: P.Parsec String u String
stringLiteral = PT.stringLiteral tokenParser

whiteSpace :: P.Parsec String u ()
whiteSpace = PT.whiteSpace tokenParser

semi :: P.Parsec String u String
semi = PT.semi tokenParser

colon :: P.Parsec String u String
colon = PT.colon tokenParser

dot :: P.Parsec String u String
dot = PT.dot tokenParser

comma :: P.Parsec String u String
comma = PT.comma tokenParser

tick :: P.Parsec String u Char
tick = lexeme $ P.char '`'

pipe :: P.Parsec String u Char
pipe = lexeme $ P.char '|'

natural :: P.Parsec String u Integer
natural = PT.natural tokenParser

squares :: P.Parsec String ParseState a -> P.Parsec String ParseState a
squares = P.between (lexeme $ P.char '[') (lexeme $ indented *> P.char ']') . (indented *>)

parens :: P.Parsec String ParseState a -> P.Parsec String ParseState a
parens = P.between (lexeme $ P.char '(') (lexeme $ indented *> P.char ')') . (indented *>)

braces :: P.Parsec String ParseState a -> P.Parsec String ParseState a
braces = P.between (lexeme $ P.char '{') (lexeme $ indented *> P.char '}') . (indented *>)

angles :: P.Parsec String ParseState a -> P.Parsec String ParseState a
angles = P.between (lexeme $ P.char '<') (lexeme $ indented *> P.char '>') . (indented *>)

sepBy :: P.Parsec String ParseState a -> P.Parsec String ParseState sep -> P.Parsec String ParseState [a]
sepBy p s = P.sepBy (indented *> p) (indented *> s)

sepBy1 :: P.Parsec String ParseState a -> P.Parsec String ParseState sep -> P.Parsec String ParseState [a]
sepBy1 p s = P.sepBy1 (indented *> p) (indented *> s)

semiSep :: P.Parsec String ParseState a -> P.Parsec String ParseState [a]
semiSep = flip sepBy semi

semiSep1 :: P.Parsec String ParseState a -> P.Parsec String ParseState [a]
semiSep1 = flip sepBy1 semi

commaSep :: P.Parsec String ParseState a -> P.Parsec String ParseState [a]
commaSep = flip sepBy comma

commaSep1 :: P.Parsec String ParseState a -> P.Parsec String ParseState [a]
commaSep1 = flip sepBy1 comma

properName :: P.Parsec String u ProperName
properName = lexeme $ ProperName <$> P.try ((:) <$> P.upper <*> many (PT.identLetter langDef) P.<?> "name")

parseQualified :: P.Parsec String ParseState a -> P.Parsec String ParseState (Qualified a)
parseQualified parser = part global
  where
  part path = (do name <- P.try (properName <* delimiter)
                  part (subModule path name))
              <|> (Qualified path <$> P.try parser)
  delimiter = indented *> dot

integerOrFloat :: P.Parsec String u (Either Integer Double)
integerOrFloat = (Left <$> P.try (PT.natural tokenParser) <|>
                  Right <$> P.try (PT.float tokenParser)) P.<?> "number"

augment :: P.Stream s m t => P.ParsecT s u m a -> P.ParsecT s u m b -> (a -> b -> a) -> P.ParsecT s u m a
augment p q f = flip (maybe id $ flip f) <$> p <*> P.optionMaybe q

fold :: P.Stream s m t => P.ParsecT s u m a -> P.ParsecT s u m b -> (a -> b -> a) -> P.ParsecT s u m a
fold first more combine = do
  a <- first
  bs <- P.many more
  return $ foldl combine a bs

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

operatorOrBuiltIn :: P.Parsec String u String
operatorOrBuiltIn = P.try operator <|> P.choice (map (\s -> P.try (reservedOp s) >> return s) builtInOperators)

parseIdent :: P.Parsec String ParseState Ident
parseIdent = (Ident <$> identifier) <|> (Op <$> parens operatorOrBuiltIn)

parseIdentInfix :: P.Parsec String ParseState (Qualified Ident)
parseIdentInfix = (P.between tick tick (parseQualified (Ident <$> identifier))) <|> parseQualified (Op <$> operatorOrBuiltIn)

mark :: P.Parsec String ParseState a -> P.Parsec String ParseState a
mark p = do
  current <- indentationLevel <$> P.getState
  pos <- P.sourceColumn <$> P.getPosition
  P.modifyState $ \st -> st { indentationLevel = pos }
  a <- p
  P.modifyState $ \st -> st { indentationLevel = current }
  return a

checkIndentation :: (P.Column -> P.Column -> Bool) -> P.Parsec String ParseState ()
checkIndentation rel = do
  col <- P.sourceColumn <$> P.getPosition
  current <- indentationLevel <$> P.getState
  guard (col `rel` current)

indented :: P.Parsec String ParseState ()
indented = checkIndentation (>) P.<?> "indentation"

same :: P.Parsec String ParseState ()
same = checkIndentation (==) P.<?> "no indentation"

runIndentParser :: P.Parsec String ParseState a -> String -> Either P.ParseError a
runIndentParser p = P.runParser p (ParseState 0) ""
