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
import qualified Data.Map as M
import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P

import PureScript.Values
import PureScript.Types
import PureScript.Parser.State
import PureScript.Parser.Common
import PureScript.Declarations
import PureScript.Parser.Values
import PureScript.Parser.Types
import PureScript.Parser.Kinds

parseDataDeclaration :: P.Parsec String ParseState Declaration
parseDataDeclaration = do
  reserved "data"
  name <- indented *> properName
  tyArgs <- many (indented *> identifier)
  lexeme $ indented *> P.char '='
  ctors <- P.sepBy1 ((,) <$> (indented *> properName) <*> P.optionMaybe parseType) (lexeme $ indented *> P.char '|')
  return $ DataDeclaration name tyArgs ctors

parseTypeDeclaration :: P.Parsec String ParseState Declaration
parseTypeDeclaration =
  TypeDeclaration <$> parseIdent
                  <*> (lexeme (indented *> P.string "::") *> parsePolyType)

parseTypeSynonymDeclaration :: P.Parsec String ParseState Declaration
parseTypeSynonymDeclaration =
  TypeSynonymDeclaration <$> (reserved "type" *> indented *> properName)
                         <*> many (indented *> identifier)
                         <*> (lexeme (indented *> P.char '=') *> parseType)

parseValueDeclaration :: P.Parsec String ParseState Declaration
parseValueDeclaration =
  ValueDeclaration <$> parseIdent
                   <*> (lexeme (indented *> P.char '=') *> parseValue)

parseExternDeclaration :: P.Parsec String ParseState Declaration
parseExternDeclaration = reserved "extern" *> indented *>
  (ExternDataDeclaration <$> (reserved "data" *> indented *> properName)
                        <*> (lexeme (indented *> P.string "::") *> parseKind)
   <|> ExternDeclaration <$> parseIdent
                        <*> (lexeme (indented *> P.string "::") *> parsePolyType))

parseAssociativity :: P.Parsec String ParseState Associativity
parseAssociativity =
  (reserved "infixl" >> return Infixl) <|>
  (reserved "infixr" >> return Infixr)

parseFixity :: P.Parsec String ParseState Fixity
parseFixity = Fixity <$> parseAssociativity <*> (indented *> natural)

parseFixityDeclaration :: P.Parsec String ParseState Declaration
parseFixityDeclaration = do
  fixity <- parseFixity
  indented
  name <- operator
  current <- fixities <$> P.getState
  when (name `M.member` current) (P.unexpected $ "redefined fixity for " ++ show name)
  P.modifyState $ \st -> st { fixities = M.insert name fixity current }
  return $ FixityDeclaration fixity name

parseDeclaration :: P.Parsec String ParseState Declaration
parseDeclaration = P.choice $ map P.try
                   [ parseDataDeclaration
                   , parseTypeDeclaration
                   , parseTypeSynonymDeclaration
                   , parseValueDeclaration
                   , parseExternDeclaration
                   , parseFixityDeclaration ]

parseDeclarations :: P.Parsec String ParseState [Declaration]
parseDeclarations = whiteSpace *> mark (same *> P.many parseDeclaration) <* P.eof
