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

import PureScript.Values
import PureScript.Types
import PureScript.Parser.Common
import PureScript.Declarations
import PureScript.Parser.Values
import PureScript.Parser.Types

parseDataDeclaration :: P.Parsec String P.Column Declaration
parseDataDeclaration = do
  reserved "data"
  name <- indented *> properName
  tyArgs <- many (indented *> identifier)
  lexeme $ indented *> P.char '='
  ctors <- P.sepBy1 ((,) <$> (indented *> properName) <*> P.optionMaybe parseType) (lexeme $ indented *> P.char '|')
  return $ DataDeclaration name tyArgs ctors

parseTypeDeclaration :: P.Parsec String P.Column Declaration
parseTypeDeclaration =
  TypeDeclaration <$> parseIdent
                  <*> (lexeme (indented *> P.string "::") *> parsePolyType)

parseTypeSynonymDeclaration :: P.Parsec String P.Column Declaration
parseTypeSynonymDeclaration =
  TypeSynonymDeclaration <$> (reserved "type" *> indented *> properName)
                         <*> many (indented *> identifier)
                         <*> (lexeme (indented *> P.char '=') *> parseType)

parseValueDeclaration :: P.Parsec String P.Column Declaration
parseValueDeclaration =
  ValueDeclaration <$> parseIdent
                   <*> (lexeme (indented *> P.char '=') *> parseValue)

parseExternDeclaration :: P.Parsec String P.Column Declaration
parseExternDeclaration =
  ExternDeclaration <$> (reserved "extern" *> indented *> parseIdent)
                    <*> (lexeme (indented *> P.string "::") *> parsePolyType)

parseDeclaration :: P.Parsec String P.Column Declaration
parseDeclaration = P.choice $ map P.try
                   [ parseDataDeclaration
                   , parseTypeDeclaration
                   , parseTypeSynonymDeclaration
                   , parseValueDeclaration
                   , parseExternDeclaration ]

parseDeclarations :: P.Parsec String P.Column [Declaration]
parseDeclarations = whiteSpace *> mark (same *> P.many parseDeclaration) <* P.eof
