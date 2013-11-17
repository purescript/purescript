-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Parser.Declarations
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

module Language.PureScript.Parser.Declarations (
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

import Language.PureScript.Names
import Language.PureScript.Values
import Language.PureScript.Types
import Language.PureScript.Parser.State
import Language.PureScript.Parser.Common
import Language.PureScript.Declarations
import Language.PureScript.Parser.Values
import Language.PureScript.Parser.Types
import Language.PureScript.Parser.Kinds

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
  TypeDeclaration <$> P.try (parseIdent <* lexeme (indented *> P.string "::"))
                  <*> parsePolyType

parseTypeSynonymDeclaration :: P.Parsec String ParseState Declaration
parseTypeSynonymDeclaration =
  TypeSynonymDeclaration <$> (P.try (reserved "type") *> indented *> properName)
                         <*> many (indented *> identifier)
                         <*> (lexeme (indented *> P.char '=') *> parseType)

parseValueDeclaration :: P.Parsec String ParseState Declaration
parseValueDeclaration =
  ValueDeclaration <$> P.try (parseIdent <* lexeme (indented *> P.char '='))
                   <*> parseValue

parseExternDeclaration :: P.Parsec String ParseState Declaration
parseExternDeclaration = P.try (reserved "foreign") *> indented *> (reserved "import") *> indented *>
   (ExternDataDeclaration <$> (P.try (reserved "data") *> indented *> properName)
                             <*> (lexeme (indented *> P.string "::") *> parseKind)
   <|> ExternDeclaration <$> parseIdent
                        <*> (lexeme (indented *> P.string "::") *> parsePolyType)
   <|> ExternMemberDeclaration <$> (P.try (reserved "member") *> indented *> stringLiteral)
                        <*> (indented *> parseIdent)
                        <*> (lexeme (indented *> P.string "::") *> parsePolyType))

parseAssociativity :: P.Parsec String ParseState Associativity
parseAssociativity =
  (P.try (reserved "infixl") >> return Infixl) <|>
  (P.try (reserved "infixr") >> return Infixr)

parseFixity :: P.Parsec String ParseState Fixity
parseFixity = Fixity <$> parseAssociativity <*> (indented *> natural)

parseFixityDeclaration :: P.Parsec String ParseState Declaration
parseFixityDeclaration = do
  fixity <- parseFixity
  indented
  name <- operator
  return $ FixityDeclaration fixity name

parseModuleDeclaration :: P.Parsec String ParseState Declaration
parseModuleDeclaration = do
  reserved "module"
  indented
  name <- properName
  lexeme $ P.string "where"
  decls <- mark (P.many (same *> parseDeclaration))
  return $ ModuleDeclaration name decls

parseImportDeclaration :: P.Parsec String ParseState Declaration
parseImportDeclaration = do
  reserved "import"
  indented
  segments <- P.sepBy1 properName (lexeme $ P.char '.')
  idents <- P.optionMaybe $ do
    lexeme $ indented *> P.char '('
    idents <- P.sepBy1 parseIdent (lexeme $ indented *> P.char ',')
    lexeme $ indented *> P.char ')'
    return idents
  let modulePath = mkModulePath (ModulePath [head segments]) (tail segments)
  return $ ImportDeclaration modulePath idents
 where mkModulePath path (s:ss) = mkModulePath (subModule path s) ss
       mkModulePath path _      = path 

parseDeclaration :: P.Parsec String ParseState Declaration
parseDeclaration = P.choice
                   [ parseDataDeclaration
                   , parseTypeDeclaration
                   , parseTypeSynonymDeclaration
                   , parseValueDeclaration
                   , parseExternDeclaration
                   , parseFixityDeclaration
                   , parseModuleDeclaration
                   , parseImportDeclaration ] P.<?> "declaration"

parseDeclarations :: P.Parsec String ParseState [Declaration]
parseDeclarations = whiteSpace *> mark (P.many (same *> parseDeclaration)) <* P.eof
