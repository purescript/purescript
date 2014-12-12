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

import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P
import qualified Text.Parsec.Error as P
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
  | Newline Int
  | ShouldIndent Int
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
  | Symbol String
  | StringLiteral String
  | Number (Either Integer Double)
  deriving (Eq, Ord)

instance Show Token where
  show LParen            = "("
  show RParen            = ")"
  show LBrace            = "{"
  show RBrace            = "}"
  show LSquare           = "["
  show RSquare           = "]"
  show LArrow            = "<-"
  show RArrow            = "->"
  show LFatArrow         = "<="
  show RFatArrow         = "=>"
  show Colon             = ":"
  show DoubleColon       = "::"
  show Equals            = "="
  show Pipe              = "|"
  show Tick              = "`"
  show Dot               = "."
  show Comma             = ","
  show Semi              = ";"
  show At                = "@"
  show (Newline _)       = "newline"
  show (ShouldIndent _)  = "indentation marker"
  show (LName s)         = show s
  show (UName s)         = show s
  show (Symbol s)        = show s
  show (StringLiteral s) = show s
  show (Number n)        = either show show n 

data PositionedToken = PositionedToken
  { ptSourcePos :: P.SourcePos
  , ptToken     :: Token
  , ptComments  :: [Comment]
  } deriving (Eq)
  
instance Show PositionedToken where
  show = show . ptToken

lex :: String -> Either P.ParseError [PositionedToken]
lex input = do
  ts <- P.parse parseTokens "" input
  let annot = insertNewlines 1 ts
  toInsensitive annot
      
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
  , UName         <$> parseUName 
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

insertNewlines :: Int -> [PositionedToken] -> [PositionedToken]
insertNewlines ref  [] = [PositionedToken (P.newPos "" ref 1) (Newline 0) []]
insertNewlines _   (t1@PositionedToken { ptToken = LName s } : t2@PositionedToken { ptSourcePos = pos } : ts) 
  | shouldIndent s && ptToken t2 /= LBrace = t1 : t1 { ptToken = ShouldIndent (P.sourceColumn pos), ptComments = [] } : t2 : insertNewlines (P.sourceLine pos) ts
insertNewlines ref (t@PositionedToken { ptSourcePos = pos } : ts) 
  | P.sourceLine pos > ref = t { ptToken = Newline (P.sourceColumn pos), ptComments = [] } : t : insertNewlines (P.sourceLine pos) ts
  | otherwise      = t : insertNewlines ref ts

-- |
-- The L function, see the Haskell 98 report, appendix B
-- Turns indentation-sensitive token streams into indentation-insensitive streams, 
-- by inserting explicit braces.
--
toInsensitive :: [PositionedToken] -> Either P.ParseError [PositionedToken]
toInsensitive = l []
  where
  l :: [Int] -> [PositionedToken] -> Either P.ParseError [PositionedToken]
  l (m : ms) ((t@PositionedToken{ ptToken = Newline n })      : ts)  | m == n = cons (t { ptToken = Semi })   $ l (m : ms)     ts
                                                                     | n < m  = cons (t { ptToken = RBrace }) $ l ms           (t : ts)
  l ms       (PositionedToken{ ptToken = Newline _ }          : ts)           =                                 l ms           ts
  l (m : ms) ((t@PositionedToken{ ptToken = ShouldIndent n }) : ts)  | n > m  = cons (t { ptToken = LBrace }) $ l (n : m : ms) ts
  l []       ((t@PositionedToken{ ptToken = ShouldIndent n }) : ts)  | n > 0  = cons (t { ptToken = LBrace }) $ l [n] ts
  l ms       ((t@PositionedToken{ ptToken = ShouldIndent n }) : ts)           = cons (t { ptToken = LBrace, ptComments = [] }) $ 
                                                                                  cons (t { ptToken = RBrace, ptComments = [] }) $ 
                                                                                    l ms           (t { ptToken = Newline n } : ts)
  l (0 : ms) ((t@PositionedToken{ ptToken = RBrace })         : ts)           = cons t                        $ l ms           ts
  l _        (t@PositionedToken{ ptToken = RBrace }           : _)            = Left $ P.newErrorMessage (P.Message "Unexpected }") (ptSourcePos t)
  l ms       ((t@PositionedToken{ ptToken = LBrace })         : ts)           = cons (t { ptToken = LBrace }) $ l (0 : ms)     ts
  l ms       (t : ts)                                                         = cons t                        $ l ms           ts
  l []       []                                                               = return []
  l _        _                                                                = error "Invalid input to j"

cons :: (Applicative f) => a -> f [a] -> f [a]
cons x xs = (:) x <$> xs

shouldIndent :: String -> Bool
shouldIndent "of" = True
shouldIndent "do" = True
shouldIndent "where" = True
shouldIndent "let" = True
shouldIndent _ = False
 
type TokenParser u a = P.Parsec [PositionedToken] u a
 
token :: (Token -> Maybe a) -> TokenParser u a
token f = P.token show ptSourcePos (f . ptToken)

match :: Token -> TokenParser u ()
match tok = token (\tok' -> if tok == tok' then Just () else Nothing) P.<?> show tok

lparen :: TokenParser u ()
lparen = match LParen

rparen :: TokenParser u ()
rparen = match RParen

parens :: TokenParser u a -> TokenParser u a
parens = P.between lparen rparen

lbrace :: TokenParser u ()
lbrace = match LBrace

rbrace :: TokenParser u ()
rbrace = match RBrace

braces :: TokenParser u a -> TokenParser u a
braces = P.between lbrace rbrace

lsquare :: TokenParser u ()
lsquare = match LSquare

rsquare :: TokenParser u ()
rsquare = match RSquare

squares :: TokenParser u a -> TokenParser u a
squares = P.between lsquare rsquare

larrow :: TokenParser u ()
larrow = match LArrow

rarrow :: TokenParser u ()
rarrow = match RArrow

lfatArrow :: TokenParser u ()
lfatArrow = match LFatArrow

rfatArrow :: TokenParser u ()
rfatArrow = match RFatArrow

colon :: TokenParser u ()
colon = match Colon

doubleColon :: TokenParser u ()
doubleColon = match DoubleColon

equals :: TokenParser u ()
equals = match Equals

pipe :: TokenParser u ()
pipe = match Pipe

tick :: TokenParser u ()
tick = match Tick

dot :: TokenParser u ()
dot = match Dot

comma :: TokenParser u ()
comma = match Comma

semi :: TokenParser u ()
semi = match Semi

at :: TokenParser u ()
at = match At 

-- |
-- Parse zero or more values separated by semicolons
--
semiSep :: TokenParser u a -> TokenParser u [a]
semiSep = flip P.sepBy semi

-- |
-- Parse one or more values separated by semicolons
--
semiSep1 :: TokenParser u a -> TokenParser u [a]
semiSep1 = flip P.sepBy1 semi

-- |
-- Parse zero or more values separated by commas
--
commaSep :: TokenParser u a -> TokenParser u [a]
commaSep = flip P.sepBy comma

-- |
-- Parse one or more values separated by commas
--
commaSep1 :: TokenParser u a -> TokenParser u [a]
commaSep1 = flip P.sepBy1 comma

lname :: TokenParser u String
lname = token go P.<?> "identifier"
  where
  go (LName s) = Just s
  go _ = Nothing

reserved :: String -> TokenParser u ()
reserved s = token go P.<?> show s
  where
  go (LName s') | s == s' = Just ()
  go _ = Nothing

uname :: TokenParser u String
uname = token go P.<?> "proper name"
  where
  go (UName s) = Just s
  go _ = Nothing

uname' :: String -> TokenParser u ()
uname' s = token go P.<?> show s
  where
  go (UName s') | s == s' = Just ()
  go _ = Nothing
  
symbol :: TokenParser u String
symbol = token go P.<?> "symbol"
  where
  go (Symbol s) = Just s
  go Colon      = Just ":"
  go LFatArrow  = Just "<="
  go _ = Nothing

symbol' :: String -> TokenParser u ()
symbol' s = token go P.<?> show s
  where
  go (Symbol s') | s == s'   = Just ()
  go Colon       | s == ":"  = Just ()
  go LFatArrow   | s == "<=" = Just ()
  go _ = Nothing
  
stringLiteral :: TokenParser u String
stringLiteral = token go P.<?> "string literal"
  where
  go (StringLiteral s) = Just s
  go _ = Nothing

number :: TokenParser u (Either Integer Double)
number = token go P.<?> "number"
  where
  go (Number n) = Just n
  go _ = Nothing

natural ::  TokenParser u Integer
natural = token go P.<?> "natural"
  where
  go (Number (Left n)) = Just n
  go _ = Nothing

identifier :: TokenParser u String
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
  