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

module PureScript.Parser.Common where

import Control.Applicative
import qualified Text.Parsec as P
import qualified Text.Parsec.Indent as I
import qualified Text.Parsec.Token as PT
import qualified Text.Parsec.Language as PL

import PureScript.Names

langDef = PT.LanguageDef
  { PT.reservedNames = [ "case"
                       , "data"
                       , "type"
                       , "var"
                       , "while"
                       , "for"
                       , "if"
                       , "then"
                       , "else"
                       , "return"
                       , "true"
                       , "false"
                       , "extern"
                       , "forall" ]
  , PT.reservedOpNames = [ "!", "~", "-", "<=", ">=", "<", ">", "*", "/", "%", "++", "+", "<<", ">>>", ">>"
                         , "==", "!=", "&", "^", "|", "&&", "||" ]
  , PT.commentStart   = "{-"
  , PT.commentEnd     = "-}"
  , PT.commentLine    = "--"
  , PT.nestedComments = True
  , PT.identStart     = P.lower <|> P.oneOf "_$"
  , PT.identLetter    = P.alphaNum <|> P.oneOf "_'"
  , PT.opStart        = P.oneOf ":#?@\\"
  , PT.opLetter       = P.oneOf ":!#$%&*+./<=>?@\\^|-~"
  , PT.caseSensitive  = True
  }

tokenParser = PT.makeTokenParser langDef

whiteSpace = P.skipMany $ (P.oneOf " \t" >> return ()) <|> P.try (P.skipMany1 (P.oneOf "\r\n")) >> I.indented <|> P.try lineComment <|> P.try blockComment

lineComment = do
  P.string $ PT.commentLine langDef
  P.skipMany $ P.satisfy (/= '\n')
  I.indented

blockComment = do
  P.string $ PT.commentStart langDef
  P.manyTill P.anyChar $ P.try $ P.string $ PT.commentEnd langDef
  return ()

lexeme p         = p <* whiteSpace
identifier       = lexeme $ PT.identifier        tokenParser
reserved s       = lexeme $ PT.reserved          tokenParser s
operator         = lexeme $ PT.operator          tokenParser
stringLiteral    = lexeme $ PT.stringLiteral     tokenParser
natural          = lexeme $ PT.natural           tokenParser
naturalOrFloat   = lexeme $ PT.naturalOrFloat    tokenParser

semi             = lexeme $ PT.semi              tokenParser
comma            = lexeme $ PT.comma             tokenParser
colon            = lexeme $ PT.colon             tokenParser
dot              = lexeme $ PT.dot               tokenParser

semiSep p        = P.sepBy p semi
semiSep1 p       = P.sepBy1 p semi
commaSep p       = P.sepBy p comma
commaSep1 p      = P.sepBy1 p comma

parens p         = lexeme (P.char '(') *> lexeme p <* lexeme (P.char ')')
braces p         = lexeme (P.char '{') *> lexeme p <* lexeme (P.char '}')
angles p         = lexeme (P.char '<') *> lexeme p <* lexeme (P.char '>')
squares p        = lexeme (P.char '[') *> lexeme p <* lexeme (P.char ']')

tick             = lexeme (P.char '`')

properName :: I.IndentParser String u String
properName = lexeme $ P.try properName'
  where
  properName' = (:) <$> P.upper <*> many (PT.identLetter langDef) P.<?> "name"

augment :: P.Stream s m t => P.ParsecT s u m a -> P.ParsecT s u m b -> (a -> b -> a) -> P.ParsecT s u m a
augment p q f = (flip $ maybe id $ flip f) <$> p <*> P.optionMaybe q

fold :: P.Stream s m t => P.ParsecT s u m a -> P.ParsecT s u m b -> (a -> b -> a) -> P.ParsecT s u m a
fold first more combine = do
  a <- first
  bs <- P.many more
  return $ foldl combine a bs

parseIdent :: I.IndentParser String () Ident
parseIdent = (Ident <$> identifier) <|> (Op <$> parens operator)

parseIdentInfix :: I.IndentParser String () Ident
parseIdentInfix = (Ident <$> P.between tick tick identifier) <|> (Op <$> operator)
