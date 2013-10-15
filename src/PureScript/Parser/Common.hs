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
import Control.Monad
import qualified Text.Parsec as P
import qualified Text.Parsec.Indent as I

import PureScript.Names

reservedNames :: [String]
reservedNames = [ "case"
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

reservedOpNames :: [String]
reservedOpNames = [ "!", "~", "-", "<=", ">=", "<", ">", "*", "/", "%", "++", "+", "<<", ">>>", ">>"
                  , "==", "!=", "&", "^", "|", "&&", "||" ]

identStart :: I.IndentParser String u Char
identStart = P.lower <|> P.oneOf "_$"

properNameStart :: I.IndentParser String u Char
properNameStart = P.upper

identLetter :: I.IndentParser String u Char
identLetter = P.alphaNum <|> P.oneOf "_'"

opStart :: I.IndentParser String u Char
opStart = P.oneOf ":!#$%&*+/<=>?@^|-~"

opLetter :: I.IndentParser String u Char
opLetter = P.oneOf ":#$%&*+./<=>?@^|"

whiteSpace :: I.IndentParser String u ()
whiteSpace = P.skipMany $ P.choice
  [ P.oneOf " \t" >> return ()
  , P.try $ P.skipMany1 (P.oneOf "\r\n") >> P.skipMany1 (P.oneOf " \t") >> I.indented
  , P.try lineComment
  , P.try blockComment ]

lineComment :: I.IndentParser String u ()
lineComment = do
  P.string "--"
  P.skipMany $ P.satisfy (/= '\n')
  return ()

blockComment :: I.IndentParser String u ()
blockComment = do
  P.string "{--"
  P.manyTill P.anyChar $ P.try $ P.string "--}"
  return ()

lexeme :: I.IndentParser String u a -> I.IndentParser String u a
lexeme p = p <* whiteSpace

identifier :: I.IndentParser String u String
identifier = lexeme $ do
  name <- (:) <$> identStart <*> P.many identLetter
  when (name `elem` reservedNames) $ P.unexpected ("reserved word " ++ show name)
  return name

properName :: I.IndentParser String u String
properName = lexeme $ do
  (:) <$> properNameStart <*> many identLetter P.<?> "name"

reserved :: String -> I.IndentParser String u String
reserved name = lexeme $ P.string name <* P.notFollowedBy identLetter

reservedOp :: String -> I.IndentParser String u String
reservedOp name = lexeme $ P.string name <* P.notFollowedBy opLetter

operator :: I.IndentParser String u String
operator = lexeme $  do
  name <- (:) <$> opStart <*> P.many opLetter
  when (name `elem` reservedOpNames) $ P.unexpected ("reserved operator " ++ show name)
  return name

stringLiteral :: I.IndentParser String u String
stringLiteral = lexeme $ P.between (P.char '"') (P.char '"') (P.many (P.noneOf "\""))

number :: I.IndentParser String u String
number = do
  intPart <- P.many1 P.digit
  fracPart <- P.option "" $ (:) <$> P.char '.'  <*> P.many1 P.digit
  expPart <- P.option "" $ (:) <$> P.oneOf "eE" <*> P.many1 P.digit
  return $ intPart ++ fracPart ++ expPart

signedNumber :: I.IndentParser String u String
signedNumber = do
  s <- P.optionMaybe $ P.oneOf "-+"
  n <- number
  return $ maybe id (:) s n

unsignedNumber :: I.IndentParser String u String
unsignedNumber = lexeme number

semi :: I.IndentParser String u Char
semi = lexeme $ P.char ';'

comma :: I.IndentParser String u Char
comma = lexeme $ P.char ','

colon :: I.IndentParser String u Char
colon = lexeme $ P.char ':'

dot :: I.IndentParser String u Char
dot = lexeme $ P.char '.'

tick :: I.IndentParser String u Char
tick = lexeme $ P.char '`'

semiSep :: I.IndentParser String u a -> I.IndentParser String u [a]
semiSep p = P.sepBy p semi

semiSep1 :: I.IndentParser String u a -> I.IndentParser String u [a]
semiSep1 p = P.sepBy1 p semi

commaSep :: I.IndentParser String u a -> I.IndentParser String u [a]
commaSep p = P.sepBy p comma

commaSep1 :: I.IndentParser String u a -> I.IndentParser String u [a]
commaSep1 p = P.sepBy1 p comma

parens :: I.IndentParser String u a -> I.IndentParser String u a
parens p = lexeme (P.char '(') *> lexeme p <* lexeme (P.char ')')

braces :: I.IndentParser String u a -> I.IndentParser String u a
braces p = lexeme (P.char '{') *> lexeme p <* lexeme (P.char '}')

angles :: I.IndentParser String u a -> I.IndentParser String u a
angles p = lexeme (P.char '<') *> lexeme p <* lexeme (P.char '>')

squares :: I.IndentParser String u a -> I.IndentParser String u a
squares p = lexeme (P.char '[') *> lexeme p <* lexeme (P.char ']')

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
