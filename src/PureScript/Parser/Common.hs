-----------------------------------------------------------------------------
--
-- Module      :  PureScript.Parser.Common
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

module PureScript.Parser.Common where

import Data.Char (isSpace)
import Control.Applicative
import Control.Monad
import Control.Monad.State
import PureScript.Parser.State
import qualified Data.Map as M
import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P
import qualified Text.Parsec.Token as PT

import PureScript.Names

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
                , "extern"
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
                , "infixr" ]

reservedOpNames :: [String]
reservedOpNames = [ "!", "~", "-", "<=", ">=", "<", ">", "*", "/", "%", "++", "+", "<<", ">>>", ">>"
                  , "==", "!=", "&", "^", "|", "&&", "||", "->" ]

identStart :: P.Parsec String u Char
identStart = P.lower <|> P.oneOf "_$"

properNameStart :: P.Parsec String u Char
properNameStart = P.upper

identLetter :: P.Parsec String u Char
identLetter = P.alphaNum <|> P.oneOf "_'"

opStart :: P.Parsec String u Char
opStart = P.oneOf "!#$%&*+/<=>?@^|-~"

opLetter :: P.Parsec String u Char
opLetter = P.oneOf ":#$%&*+./<=>?@^|"

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

tokenParser = PT.makeTokenParser langDef

lexeme           = PT.lexeme            tokenParser
identifier       = PT.identifier        tokenParser
reserved         = PT.reserved          tokenParser
reservedOp       = PT.reservedOp        tokenParser
operator         = PT.operator          tokenParser
stringLiteral    = PT.stringLiteral     tokenParser
whiteSpace       = PT.whiteSpace        tokenParser
parens           = PT.parens            tokenParser
braces           = PT.braces            tokenParser
angles           = PT.angles            tokenParser
squares          = PT.squares           tokenParser
semi             = PT.semi              tokenParser
comma            = PT.comma             tokenParser
colon            = PT.colon             tokenParser
dot              = PT.dot               tokenParser
semiSep          = PT.semiSep           tokenParser
semiSep1         = PT.semiSep1          tokenParser
commaSep         = PT.commaSep          tokenParser
commaSep1        = PT.commaSep1         tokenParser
natural          = PT.natural           tokenParser

tick :: P.Parsec String u Char
tick = lexeme $ P.char '`'

properName :: P.Parsec String u String
properName = lexeme $ P.try ((:) <$> P.upper <*> many (PT.identLetter langDef) P.<?> "name")

integerOrFloat :: P.Parsec String u (Either Integer Double)
integerOrFloat = (Left <$> P.try (PT.natural tokenParser) <|>
                  Right <$> P.try (PT.float tokenParser)) P.<?> "number"

augment :: P.Stream s m t => P.ParsecT s u m a -> P.ParsecT s u m b -> (a -> b -> a) -> P.ParsecT s u m a
augment p q f = (flip $ maybe id $ flip f) <$> p <*> P.optionMaybe q

fold :: P.Stream s m t => P.ParsecT s u m a -> P.ParsecT s u m b -> (a -> b -> a) -> P.ParsecT s u m a
fold first more combine = do
  a <- first
  bs <- P.many more
  return $ foldl combine a bs

buildPostfixParser :: P.Stream s m t => [P.ParsecT s u m (a -> a)] -> P.ParsecT s u m a -> P.ParsecT s u m a
buildPostfixParser f x = fold x (P.choice (map P.try f)) (flip ($))

parseIdent :: P.Parsec String u Ident
parseIdent = (Ident <$> identifier) <|> (Op <$> parens operator)

parseIdentInfix :: P.Parsec String u Ident
parseIdentInfix = (Ident <$> P.between tick tick identifier) <|> (Op <$> operator)

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
runIndentParser p = P.runParser p (ParseState 0 M.empty) ""
