-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Parser.Lexer
-- Copyright   :  (c) Phil Freeman 2014
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- The first step in the parsing process - turns source code into a list of lexemes
--
-----------------------------------------------------------------------------

{-# LANGUAGE TupleSections #-}

module Language.PureScript.Parser.Lexer 
  ( PositionedToken(..)
  , Token()
  , TokenParser()
  , lex
  , token
  , match
  , lparen
  , rparen
  , parens
  , lbrace
  , rbrace
  , braces
  , lsquare
  , rsquare
  , squares
  , indent
  , indentAt
  , larrow 
  , rarrow 
  , lfatArrow
  , rfatArrow
  , colon
  , doubleColon
  , equals
  , pipe
  , tick
  , dot
  , comma
  , semi
  , at
  , semiSep
  , semiSep1
  , commaSep
  , commaSep1
  , lname
  , qualifier
  , uname
  , uname'
  , reserved
  , symbol
  , symbol'
  , identifier
  , stringLiteral
  , number
  , natural
  , reservedPsNames
  , opChars
  )
  where

import Prelude hiding (lex)

import Data.Functor (void)
import Data.Functor.Identity

import Control.Applicative

import Language.PureScript.Parser.State

import qualified Text.Parsec as P
import qualified Text.Parsec.Token as PT

data Comment
  = LineComment String
  | BlockComment String
  deriving (Show, Eq, Ord)
  
data Token
  = LParen
  | RParen
  | LBrace
  | RBrace
  | LSquare
  | RSquare
  | Indent Int
  | LArrow
  | RArrow
  | LFatArrow
  | RFatArrow
  | Colon
  | DoubleColon
  | Equals
  | Pipe
  | Tick
  | Dot
  | Comma
  | Semi
  | At
  | LName String
  | UName String
  | Qualifier String
  | Symbol String
  | StringLiteral String
  | Number (Either Integer Double)
  deriving (Show, Eq, Ord)
  
prettyPrintToken :: Token -> String
prettyPrintToken LParen            = "("
prettyPrintToken RParen            = ")"
prettyPrintToken LBrace            = "{"
prettyPrintToken RBrace            = "}"
prettyPrintToken LSquare           = "["
prettyPrintToken RSquare           = "]"
prettyPrintToken LArrow            = "<-"
prettyPrintToken RArrow            = "->"
prettyPrintToken LFatArrow         = "<="
prettyPrintToken RFatArrow         = "=>"
prettyPrintToken Colon             = ":"
prettyPrintToken DoubleColon       = "::"
prettyPrintToken Equals            = "="
prettyPrintToken Pipe              = "|"
prettyPrintToken Tick              = "`"
prettyPrintToken Dot               = "."
prettyPrintToken Comma             = ","
prettyPrintToken Semi              = ";"
prettyPrintToken At                = "@"
prettyPrintToken (Indent n)        = "indentation at level " ++ show n
prettyPrintToken (LName s)         = show s
prettyPrintToken (UName s)         = show s
prettyPrintToken (Qualifier _)     = "qualifier"
prettyPrintToken (Symbol s)        = s
prettyPrintToken (StringLiteral s) = show s
prettyPrintToken (Number n)        = either show show n

data PositionedToken = PositionedToken
  { ptSourcePos :: P.SourcePos
  , ptToken     :: Token
  , ptComments  :: [Comment]
  } deriving (Eq)
  
instance Show PositionedToken where
  show = show . ptToken

lex :: FilePath -> String -> Either P.ParseError [PositionedToken]
lex filePath input = insertIndents <$> P.parse parseTokens filePath input
      
parseTokens :: P.Parsec String u [PositionedToken]
parseTokens = whitespace *> P.many parsePositionedToken <* P.skipMany parseComment <* P.eof

whitespace :: P.Parsec String u ()
whitespace = P.skipMany (P.oneOf " \t\r\n")
    
parseComment :: P.Parsec String u Comment
parseComment = (BlockComment <$> blockComment <|> LineComment <$> lineComment) <* whitespace
  where
  blockComment :: P.Parsec String u String
  blockComment = P.try $ P.string "{-" *> P.manyTill P.anyChar (P.try (P.string "-}"))

  lineComment :: P.Parsec String u String
  lineComment = P.try $ P.string "--" *> P.manyTill P.anyChar (P.try (void (P.char '\n') <|> P.eof))

parsePositionedToken :: P.Parsec String u PositionedToken
parsePositionedToken = do
  pos <- P.getPosition
  comments <- P.many parseComment
  tok <- parseToken
  return $ PositionedToken pos tok comments

parseToken :: P.Parsec String u Token
parseToken = P.choice
  [ P.try $ P.string "<-" *> P.notFollowedBy symbolChar *> pure LArrow    
  , P.try $ P.string "<=" *> P.notFollowedBy symbolChar *> pure LFatArrow
  , P.try $ P.string "->" *> P.notFollowedBy symbolChar *> pure RArrow    
  , P.try $ P.string "=>" *> P.notFollowedBy symbolChar *> pure RFatArrow 
  , P.try $ P.string "::" *> P.notFollowedBy symbolChar *> pure DoubleColon 
  , P.try $ P.char '('    *> pure LParen
  , P.try $ P.char ')'    *> pure RParen
  , P.try $ P.char '{'    *> pure LBrace
  , P.try $ P.char '}'    *> pure RBrace
  , P.try $ P.char '['    *> pure LSquare
  , P.try $ P.char ']'    *> pure RSquare
  , P.try $ P.char '`'    *> pure Tick
  , P.try $ P.char ','    *> pure Comma
  , P.try $ P.char '='    *> P.notFollowedBy symbolChar *> pure Equals
  , P.try $ P.char ':'    *> P.notFollowedBy symbolChar *> pure Colon
  , P.try $ P.char '|'    *> P.notFollowedBy symbolChar *> pure Pipe
  , P.try $ P.char '.'    *> P.notFollowedBy symbolChar *> pure Dot
  , P.try $ P.char ';'    *> P.notFollowedBy symbolChar *> pure Semi
  , P.try $ P.char '@'    *> P.notFollowedBy symbolChar *> pure At
  , LName         <$> parseLName 
  , do uName <- parseUName
       (Qualifier uName <$ P.char '.') <|> pure (UName uName)
  , Symbol        <$> parseSymbol 
  , StringLiteral <$> parseStringLiteral
  , Number        <$> parseNumber
  ] <* whitespace

  where
  parseLName :: P.Parsec String u String
  parseLName = (:) <$> identStart <*> P.many identLetter
  
  parseUName :: P.Parsec String u String
  parseUName = (:) <$> P.upper <*> P.many P.alphaNum
  
  parseSymbol :: P.Parsec String u String
  parseSymbol = P.many1 symbolChar
  
  identStart :: P.Parsec String u Char
  identStart = P.lower <|> P.oneOf "_"
  
  identLetter :: P.Parsec String u Char
  identLetter = P.alphaNum <|> P.oneOf "_'"
  
  symbolChar :: P.Parsec String u Char
  symbolChar = P.oneOf opChars
  
  parseStringLiteral :: P.Parsec String u String
  parseStringLiteral = blockString <|> PT.stringLiteral tokenParser
    where 
    delimeter   = P.try (P.string "\"\"\"")
    blockString = delimeter >> P.manyTill P.anyChar delimeter
  
  parseNumber :: P.Parsec String u (Either Integer Double)
  parseNumber = (Right <$> P.try (PT.float tokenParser) <|>
                 Left <$> P.try (PT.natural tokenParser)) P.<?> "number"

-- |
-- We use Text.Parsec.Token to implement the string and number lexemes
--
langDef :: PT.GenLanguageDef String u Identity
langDef = PT.LanguageDef
  { PT.reservedNames   = []
  , PT.reservedOpNames = []
  , PT.commentStart    = ""
  , PT.commentEnd      = ""
  , PT.commentLine     = ""
  , PT.nestedComments  = True
  , PT.identStart      = fail "Identifiers not supported"
  , PT.identLetter     = fail "Identifiers not supported"
  , PT.opStart         = fail "Operators not supported"
  , PT.opLetter        = fail "Operators not supported"
  , PT.caseSensitive   = True
  }

-- |
-- A token parser based on the language definition
--
tokenParser :: PT.GenTokenParser String u Identity
tokenParser = PT.makeTokenParser langDef

insertIndents :: [PositionedToken] -> [PositionedToken]
insertIndents = findOutdents True [1]
  where
  findOutdents :: Bool -> [P.Column] -> [PositionedToken] -> [PositionedToken]
  findOutdents _     _          [] = []
  findOutdents _     (c : cs)   ts@(PositionedToken { ptSourcePos = pos } : _) 
    | unindenting = findOutdents False cs ts
    where
    unindenting = P.sourceColumn pos < c 
  findOutdents False cs@(c : _) (t@PositionedToken { ptSourcePos = pos } : ts) 
    | isMatching = t { ptToken = Indent (P.sourceColumn pos), ptComments = [] } : findIndents cs (t : ts)
    where
    isMatching = P.sourceColumn pos == c   
  findOutdents _     cs         (t : ts) = findIndents cs (t : ts)
  
  findIndents :: [P.Column] -> [PositionedToken] -> [PositionedToken]
  findIndents _          [] = []
  findIndents cs@(c : _) (t@PositionedToken { ptToken = LName s } : ts@(t'@PositionedToken { ptSourcePos = pos } : _)) 
    | shouldIndent && indenting = t : t' { ptToken = Indent (P.sourceColumn pos), ptComments = [] } : findOutdents True (P.sourceColumn pos : cs) ts
    where
    shouldIndent = s `elem` ["of", "do", "where", "let"]
    indenting    = let p = P.sourceColumn pos in p == 1 || p > c
  findIndents cs         (t : ts) = t : findOutdents False cs ts
 
type TokenParser a = P.Parsec [PositionedToken] ParseState a
 
token :: (Token -> Maybe a) -> TokenParser a
token f = P.token (prettyPrintToken . ptToken) ptSourcePos (f . ptToken)

match :: Token -> TokenParser ()
match tok = token (\tok' -> if tok == tok' then Just () else Nothing) P.<?> prettyPrintToken tok

lparen :: TokenParser ()
lparen = match LParen

rparen :: TokenParser ()
rparen = match RParen

parens :: TokenParser a -> TokenParser a
parens = P.between lparen rparen

lbrace :: TokenParser ()
lbrace = match LBrace

rbrace :: TokenParser ()
rbrace = match RBrace

braces :: TokenParser a -> TokenParser a
braces = P.between lbrace rbrace

lsquare :: TokenParser ()
lsquare = match LSquare

rsquare :: TokenParser ()
rsquare = match RSquare

squares :: TokenParser a -> TokenParser a
squares = P.between lsquare rsquare

indent :: TokenParser Int
indent = token go P.<?> "indentation"
  where
  go (Indent n) = Just n
  go _ = Nothing

indentAt :: P.Column -> TokenParser ()
indentAt n = token go P.<?> "indentation at level " ++ show n
  where
  go (Indent n') | n == n' = Just ()
  go _ = Nothing

larrow :: TokenParser ()
larrow = match LArrow

rarrow :: TokenParser ()
rarrow = match RArrow

lfatArrow :: TokenParser ()
lfatArrow = match LFatArrow

rfatArrow :: TokenParser ()
rfatArrow = match RFatArrow

colon :: TokenParser ()
colon = match Colon

doubleColon :: TokenParser ()
doubleColon = match DoubleColon

equals :: TokenParser ()
equals = match Equals

pipe :: TokenParser ()
pipe = match Pipe

tick :: TokenParser ()
tick = match Tick

dot :: TokenParser ()
dot = match Dot

comma :: TokenParser ()
comma = match Comma

semi :: TokenParser ()
semi = match Semi

at :: TokenParser ()
at = match At 

-- |
-- Parse zero or more values separated by semicolons
--
semiSep :: TokenParser a -> TokenParser [a]
semiSep = flip P.sepBy semi

-- |
-- Parse one or more values separated by semicolons
--
semiSep1 :: TokenParser a -> TokenParser [a]
semiSep1 = flip P.sepBy1 semi

-- |
-- Parse zero or more values separated by commas
--
commaSep :: TokenParser a -> TokenParser [a]
commaSep = flip P.sepBy comma

-- |
-- Parse one or more values separated by commas
--
commaSep1 :: TokenParser a -> TokenParser [a]
commaSep1 = flip P.sepBy1 comma

lname :: TokenParser String
lname = token go P.<?> "identifier"
  where
  go (LName s) = Just s
  go _ = Nothing

qualifier :: TokenParser String
qualifier = token go P.<?> "qualifier"
  where
  go (Qualifier s) = Just s
  go _ = Nothing

reserved :: String -> TokenParser ()
reserved s = token go P.<?> show s
  where
  go (LName s') | s == s' = Just ()
  go _ = Nothing

uname :: TokenParser String
uname = token go P.<?> "proper name"
  where
  go (UName s) = Just s
  go _ = Nothing

uname' :: String -> TokenParser ()
uname' s = token go P.<?> show s
  where
  go (UName s') | s == s' = Just ()
  go _ = Nothing
  
symbol :: TokenParser String
symbol = token go P.<?> "symbol"
  where
  go (Symbol s) = Just s
  go Colon      = Just ":"
  go LFatArrow  = Just "<="
  go _ = Nothing

symbol' :: String -> TokenParser ()
symbol' s = token go P.<?> show s
  where
  go (Symbol s') | s == s'   = Just ()
  go Colon       | s == ":"  = Just ()
  go LFatArrow   | s == "<=" = Just ()
  go _ = Nothing
  
stringLiteral :: TokenParser String
stringLiteral = token go P.<?> "string literal"
  where
  go (StringLiteral s) = Just s
  go _ = Nothing

number :: TokenParser (Either Integer Double)
number = token go P.<?> "number"
  where
  go (Number n) = Just n
  go _ = Nothing

natural :: TokenParser Integer
natural = token go P.<?> "natural"
  where
  go (Number (Left n)) = Just n
  go _ = Nothing

identifier :: TokenParser String
identifier = token go P.<?> "identifier"
  where
  go (LName s) | s `notElem` reservedPsNames = Just s
  go _ = Nothing
  
-- |
-- A list of purescript reserved identifiers
--
reservedPsNames :: [String]
reservedPsNames = [ "data"
                  , "newtype"
                  , "type"
                  , "foreign"
                  , "import"
                  , "infixl"
                  , "infixr"
                  , "infix"
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
-- The characters allowed for use in operators
--
opChars :: [Char]
opChars = ":!#$%&*+./<=>?@\\^|-~"
  