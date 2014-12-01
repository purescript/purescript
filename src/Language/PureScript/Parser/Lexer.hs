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
import Data.Functor.Identity

import Control.Applicative

import Language.PureScript.Parser.Common

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

lex :: String -> Either String [PositionedToken]
lex input = 
  case P.parse parseTokens "" input of
    Left err -> Left $ show err
    Right ts -> j [] [] (insertNewlines 1 ts)
  where
      
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
    return $ PositionedToken (P.sourceLine pos) (P.sourceColumn pos) tok comments
  
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
    , P.try $ P.char '<'    *> P.notFollowedBy symbolChar *> pure LAngle
    , P.try $ P.char '>'    *> P.notFollowedBy symbolChar *> pure RAngle
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
    , Natural       <$> parseNatural
    , ANumber       <$> parseANumber
    ] <* whitespace
  
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
    parseStringLiteral = blockString <|> PT.stringLiteral tokenParser
      where 
      delimeter   = P.try (P.string "\"\"\"")
      blockString = delimeter >> P.manyTill P.anyChar delimeter
    
    parseNatural :: P.Parsec String u Integer
    parseNatural = P.try $ PT.natural tokenParser
    
    parseANumber :: P.Parsec String u (Either Integer Double)
    parseANumber = (Right <$> P.try (PT.float tokenParser) <|>
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
