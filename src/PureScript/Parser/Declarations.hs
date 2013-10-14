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
import Control.Arrow (Arrow(..))
import Control.Monad.State
import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P
import qualified Text.Parsec.Indent as I

import PureScript.Values
import PureScript.Types
import PureScript.Parser.Common
import PureScript.Declarations
import PureScript.Parser.Values
import PureScript.Parser.Types

parseDataDeclaration :: I.IndentParser String () Declaration
parseDataDeclaration = do
  reserved "data"
  name <- properName
  tyArgs <- many identifier
  lexeme $ P.char '='
  ctors <- P.sepBy1 ((,) <$> properName <*> P.optionMaybe parseType) (lexeme $ P.char '|')
  return $ DataDeclaration name tyArgs ctors

parseTypeDeclaration :: I.IndentParser String () Declaration
parseTypeDeclaration = TypeDeclaration <$> parseIdent <*> (lexeme (P.string "::") *> parsePolyType)

parseTypeSynonymDeclaration :: I.IndentParser String () Declaration
parseTypeSynonymDeclaration = TypeSynonymDeclaration <$> (reserved "type" *> properName) <*> many identifier <*> (lexeme (P.char '=') *> parseType)

parseValueDeclaration :: I.IndentParser String () Declaration
parseValueDeclaration = ValueDeclaration <$> parseIdent <*> (lexeme (P.char '=') *> parseValue)

parseExternDeclaration :: I.IndentParser String () Declaration
parseExternDeclaration = ExternDeclaration <$> (reserved "extern" *> parseIdent) <*> (lexeme (P.string "::") *> parsePolyType)

parseDeclaration :: I.IndentParser String () Declaration
parseDeclaration = P.choice $ map P.try
                   [ parseDataDeclaration
                   , parseTypeDeclaration
                   , parseTypeSynonymDeclaration
                   , parseValueDeclaration
                   , parseExternDeclaration ]

parseManyDeclarations :: I.IndentParser String () [Declaration]
parseManyDeclarations = I.block $ I.withPos parseDeclaration

parseDeclarations :: String -> Either P.ParseError [Declaration]
parseDeclarations = flip evalState (P.initialPos "Declarations") . P.runPT parseManyDeclarations () "Declarations"

