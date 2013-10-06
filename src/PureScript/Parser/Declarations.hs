-----------------------------------------------------------------------------
--
-- Module      :  PureScript.Parser.Declarations
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

module PureScript.Parser.Declarations (
    parseDeclaration,
    parseDeclarations
) where

import Data.Char
import Data.List
import Data.Maybe
import Data.Function
import Control.Applicative
import qualified Text.Parsec as P
import Control.Arrow (Arrow(..))

import PureScript.Values
import PureScript.Types
import PureScript.Parser.Common
import PureScript.Declarations
import PureScript.Parser.Values
import PureScript.Parser.Types

parseDataDeclaration :: P.Parsec String () Declaration
parseDataDeclaration = do
  reserved "data"
  name <- properName
  tyArgs <- many identifier
  lexeme $ P.char '='
  ctors <- P.sepBy1 ((,) <$> properName <*> P.optionMaybe parseType) (lexeme $ P.char '|')
  return $ DataDeclaration name tyArgs ctors

parseTypeDeclaration :: P.Parsec String () Declaration
parseTypeDeclaration = TypeDeclaration <$> identifier <*> (lexeme (P.string "::") *> parseType)

parseTypeSynonymDeclaration :: P.Parsec String () Declaration
parseTypeSynonymDeclaration = TypeSynonymDeclaration <$> (reserved "type" *> properName) <*> many identifier <*> (lexeme (P.char '=') *> parseType)

parseValueDeclaration :: P.Parsec String () Declaration
parseValueDeclaration = ValueDeclaration <$> identifier <*> (lexeme (P.char '=') *> parseValue)

parseExternDeclaration :: P.Parsec String () Declaration
parseExternDeclaration = ExternDeclaration <$> (reserved "extern" *> (identifier <|> properName)) <*> (lexeme (P.string "::") *> parseType)

parseDeclaration :: P.Parsec String () Declaration
parseDeclaration = P.choice $ map P.try
                   [ parseDataDeclaration
                   , parseTypeDeclaration
                   , parseTypeSynonymDeclaration
                   , parseValueDeclaration
                   , parseExternDeclaration ]

parseDeclarations :: String -> Either P.ParseError [Declaration]
parseDeclarations = fmap catMaybes . mapM (P.parse (whiteSpace *> (P.optionMaybe parseDeclaration <* whiteSpace) <* P.eof) "Declaration") . splitFileIntoDeclarations

isIndented :: String -> Bool
isIndented = any isSpace . take 1

splitFileIntoDeclarations :: String -> [String]
splitFileIntoDeclarations = reverse . map (dropWhile isSpace . unlines . reverse) . go [] [] . lines
  where
  go [] sss [] = sss
  go ss sss [] = ss:sss
  go ss sss (s:rest)
    | all isSpace s = go ss sss rest
    | isIndented s = go (s:ss) sss rest
    | null ss = go [s] sss rest
    | otherwise = go [s] (ss:sss) rest



