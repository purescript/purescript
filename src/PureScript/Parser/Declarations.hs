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

import PureScript.Values
import PureScript.Types
import PureScript.Parser.Common
import Control.Applicative
import qualified Text.Parsec as P
import Control.Arrow (Arrow(..))
import PureScript.Declarations
import PureScript.Parser.Values
import PureScript.Parser.Types

parseDataDeclaration :: P.Parsec String () Declaration
parseDataDeclaration = do
  reserved "data"
  name <- properName
  tyArgs <- many identifier
  lexeme $ P.char '='
  ctors <- P.sepBy1 ((,) <$> properName <*> parseType) (lexeme $ P.char '|')
  return $ DataDeclaration $ DataConstructors name tyArgs ctors

parseValueDeclaration :: P.Parsec String () Declaration
parseValueDeclaration = ValueDeclaration <$> identifier <*> (lexeme (P.char '=') *> parseValue)

parseDeclaration :: P.Parsec String () Declaration
parseDeclaration = parseDataDeclaration
                   P.<|> parseValueDeclaration

parseDeclarations :: P.Parsec String () [Declaration]
parseDeclarations = whiteSpace *> P.sepEndBy parseDeclaration semi <* P.eof
