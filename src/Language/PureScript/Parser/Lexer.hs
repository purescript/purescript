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
  , Token(..)
  , lex
  )
  where

import Prelude hiding (lex)

import Data.Functor (void)

import Control.Applicative

import Language.PureScript.Parser.Common

import qualified Text.Parsec as P

data Comment
  = LineComment String
  | BlockComment String
  deriving (Show, Eq, Ord)
  
data Token
  = LParen
  | RParen
  | LBrace
  | RBrace
  | LAngle
  | RAngle
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
  | Natural Integer
  | ANumber (Either Integer Double)
  deriving (Show, Eq, Ord)

data PositionedToken = PositionedToken
  { ptLine     :: Int
  , ptColumn   :: Int
  , ptToken    :: Token
  , ptComments :: [Comment]
  } deriving (Show, Eq)

lex :: String -> Either String ([PositionedToken], [Comment])
lex input = 
  case P.parse parseTokens "" input of
    Left err -> Left $ show err
    Right (ts, cs) -> (, cs) <$> j [] [] (insertNewlines 1 ts)
  where
      
  parseTokens :: P.Parsec String u ([PositionedToken], [Comment])
  parseTokens = (,) <$> (P.many (whitespace *> parsePositionedToken) <* P.eof)
                    <*> P.many parseComment
  
  whitespace :: P.Parsec String u ()
  whitespace = P.skipMany (P.oneOf " \t\r\n")
      
  parseComment :: P.Parsec String u Comment
  parseComment = whitespace *> (BlockComment <$> blockComment <|> LineComment <$> lineComment)
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
    return $ PositionedToken (P.sourceLine pos) (P.sourceColumn pos) tok comments
  
  parseToken :: P.Parsec String u Token
  parseToken = whitespace *> P.choice
    [ P.try $ P.char '<' *> (P.char '-' *> pure LArrow    
                             <|> P.char '=' *> pure LFatArrow 
                             <|> P.notFollowedBy symbolChar *> pure LAngle)
    , P.try $ P.char '=' *> (P.char '>' *> pure RFatArrow 
                             <|> P.notFollowedBy symbolChar *> pure Equals)
    , P.try $ P.char ':' *> (P.char ':' *> pure DoubleColon 
                             <|> P.notFollowedBy symbolChar *> pure Colon)
    , P.try $ P.string "->" *> pure RArrow
    , P.try $ P.char '('    *> P.notFollowedBy symbolChar *> pure LParen
    , P.try $ P.char ')'    *> P.notFollowedBy symbolChar *> pure RParen
    , P.try $ P.char '{'    *> P.notFollowedBy symbolChar *> pure LBrace
    , P.try $ P.char '}'    *> P.notFollowedBy symbolChar *> pure RBrace
    , P.try $ P.char '>'    *> P.notFollowedBy symbolChar *> pure RAngle
    , P.try $ P.char '['    *> P.notFollowedBy symbolChar *> pure LSquare
    , P.try $ P.char ']'    *> P.notFollowedBy symbolChar *> pure RSquare
    , P.try $ P.char ':'    *> P.notFollowedBy symbolChar *> pure Colon
    , P.try $ P.char '|'    *> P.notFollowedBy symbolChar *> pure Pipe
    , P.try $ P.char '`'    *> P.notFollowedBy symbolChar *> pure Tick
    , P.try $ P.char '.'    *> P.notFollowedBy symbolChar *> pure Dot
    , P.try $ P.char ','    *> P.notFollowedBy symbolChar *> pure Comma
    , P.try $ P.char ';'    *> P.notFollowedBy symbolChar *> pure Semi
    , P.try $ P.char '@'    *> P.notFollowedBy symbolChar *> pure At
    , LName         <$> parseLName 
    , UName         <$> parseUName 
    , Symbol        <$> parseSymbol 
    , StringLiteral <$> parseStringLiteral
    , Natural       <$> parseNatural
    , ANumber       <$> parseANumber
    ]
  
    where
    parseLName :: P.Parsec String u String
    parseLName = (:) <$> identStart <*> P.many identLetter
    
    parseUName :: P.Parsec String u String
    parseUName = (:) <$> P.upper <*> P.many P.alphaNum
    
    parseSymbol :: P.Parsec String u String
    parseSymbol = P.many1 symbolChar
    
    symbolChar :: P.Parsec String u Char
    symbolChar = P.oneOf opChars
    
    parseStringLiteral :: P.Parsec String u String
    parseStringLiteral = fail "StringLiteral not implemented"
    
    parseNatural :: P.Parsec String u Integer
    parseNatural = fail "Natural not implemented"
    
    parseANumber :: P.Parsec String u (Either Integer Double)
    parseANumber = fail "ANumber not implemented"
  
  insertNewlines :: Int -> [PositionedToken] -> [PositionedToken]
  insertNewlines _   [] = []
  insertNewlines _   (t1@PositionedToken { ptToken = LName s } : ts@(t2 : _)) 
    | shouldIndent s = t1 : t2 { ptToken = ShouldIndent (ptColumn t2), ptComments = [] } : insertNewlines (ptLine t2) ts
  insertNewlines ref (t : ts) 
    | ptLine t > ref = t { ptToken = Newline (ptColumn t), ptComments = [] } : t : insertNewlines (ptLine t) ts
    | otherwise      = t : insertNewlines ref ts
  
  j :: [Int] -> [PositionedToken] -> [PositionedToken] -> Either String [PositionedToken]
  j (m : ms) acc ((t@PositionedToken{ ptToken = Newline n })      : ts) 
    | m == n = j (m : ms) (t { ptToken = Semi } : acc) ts
  j []       acc ((t@PositionedToken{ ptToken = Newline 1 })      : ts) 
    = j [] (t { ptToken = Semi } : acc) ts
  j (m : ms) acc ((t@PositionedToken{ ptToken = Newline n })      : ts) 
    | n < m = j ms (t { ptToken = RBrace } : acc) (t : ts)
  j ms       acc (PositionedToken{ ptToken = Newline _ }          : ts) 
    = j ms acc ts
  j (m : ms) acc ((t@PositionedToken{ ptToken = ShouldIndent n }) : ts) 
    | n > m = j (n : m : ms) (t { ptToken = LBrace } : acc) ts
  j []       acc ((t@PositionedToken{ ptToken = ShouldIndent n }) : ts) 
    | n > 1 = j [n] (t { ptToken = LBrace } : acc) ts
  j ms       acc ((t@PositionedToken{ ptToken = ShouldIndent n }) : ts) 
    = j ms (t { ptToken = RBrace, ptComments = [] } : t { ptToken = LBrace, ptComments = [] } : acc) (t { ptToken = Newline n } : ts)
  j (1 : ms) acc ((t@PositionedToken{ ptToken = RBrace })         : ts) 
    = j ms (t : acc) ts 
  j _        _   (PositionedToken{ ptToken = RBrace }             : _ ) 
    = Left "Unexpected }"
  j ms       acc ((t@PositionedToken{ ptToken = LBrace })         : ts) 
    = j (1 : ms) (t { ptToken = LBrace } : acc) ts 
  j ms       acc (t : ts)                              
    = j ms (t : acc) ts
  j []       acc []                                    
    = return $ reverse acc
  j (m : ms) acc []                                    
    | m > 1 = j ms (PositionedToken 0 0 RBrace [] : acc) []
  j _        _   _                                     
    = error "Invalid input to j"

  shouldIndent :: String -> Bool
  shouldIndent "of" = True
  shouldIndent "do" = True
  shouldIndent "where" = True
  shouldIndent "let" = True
  shouldIndent _ = False
