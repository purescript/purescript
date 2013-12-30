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
    parseModule,
    parseModules
) where

import Control.Applicative
import qualified Text.Parsec as P

import Language.PureScript.Names
import Language.PureScript.Parser.State
import Language.PureScript.Parser.Common
import Language.PureScript.Declarations
import Language.PureScript.Parser.Values
import Language.PureScript.Parser.Types
import Language.PureScript.Parser.Kinds
import Language.PureScript.Values

parseDataDeclaration :: P.Parsec String ParseState Declaration
parseDataDeclaration = do
  reserved "data"
  name <- indented *> properName
  tyArgs <- many (indented *> identifier)
  lexeme $ indented *> P.char '='
  ctors <- sepBy1 ((,) <$> properName <*> P.optionMaybe (indented *> parsePolyType)) pipe
  return $ DataDeclaration name tyArgs ctors

parseTypeDeclaration :: P.Parsec String ParseState Declaration
parseTypeDeclaration =
  TypeDeclaration <$> P.try (parseIdent <* lexeme (indented *> P.string "::"))
                  <*> parsePolyType

parseTypeSynonymDeclaration :: P.Parsec String ParseState Declaration
parseTypeSynonymDeclaration =
  TypeSynonymDeclaration <$> (P.try (reserved "type") *> indented *> properName)
                         <*> many (indented *> identifier)
                         <*> (lexeme (indented *> P.char '=') *> parsePolyType)

parseValueDeclaration :: P.Parsec String ParseState Declaration
parseValueDeclaration =
  ValueDeclaration <$> parseIdent
                   <*> P.many parseTopLevelBinder
                   <*> P.optionMaybe parseGuard
                   <*> ((lexeme (indented *> P.char '=')) *> parseValue)

parseTopLevelBinder :: P.Parsec String ParseState [Binder]
parseTopLevelBinder = return <$> P.try parseBinderNoParens <|> parens (commaSep parseBinder)

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

parseImportDeclaration :: P.Parsec String ParseState Declaration
parseImportDeclaration = do
  reserved "import"
  indented
  moduleName <- ModuleName <$> properName
  idents <- P.optionMaybe $ parens $ commaSep1 parseIdent
  return $ ImportDeclaration moduleName idents

parseDeclaration :: P.Parsec String ParseState Declaration
parseDeclaration = P.choice
                   [ parseDataDeclaration
                   , parseTypeDeclaration
                   , parseTypeSynonymDeclaration
                   , parseValueDeclaration
                   , parseExternDeclaration
                   , parseFixityDeclaration
                   , parseImportDeclaration ] P.<?> "declaration"

parseModule :: P.Parsec String ParseState Module
parseModule = do
  reserved "module"
  indented
  name <- properName
  lexeme $ P.string "where"
  decls <- mark (P.many (same *> parseDeclaration))
  return $ Module name decls

parseModules :: P.Parsec String ParseState [Module]
parseModules = whiteSpace *> mark (P.many (same *> parseModule)) <* P.eof
