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
-- Parsers for module definitions and declarations
--
-----------------------------------------------------------------------------

module Language.PureScript.Parser.Declarations (
    parseDeclaration,
    parseModule,
    parseModules
) where

import Data.Maybe (isJust, fromMaybe)
import Control.Applicative
import qualified Text.Parsec as P

import Language.PureScript.Parser.State
import Language.PureScript.Parser.Common
import Language.PureScript.Declarations
import Language.PureScript.Parser.Values
import Language.PureScript.Parser.Types
import Language.PureScript.Parser.Kinds
import Language.PureScript.CodeGen.JS.AST

parseDataDeclaration :: P.Parsec String ParseState Declaration
parseDataDeclaration = do
  reserved "data"
  name <- indented *> properName
  tyArgs <- many (indented *> identifier)
  ctors <- P.option [] $ do
    _ <- lexeme $ indented *> P.char '='
    sepBy1 ((,) <$> properName <*> P.many (indented *> parseTypeAtom)) pipe
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
                   <*> P.many parseBinderNoParens
                   <*> P.optionMaybe parseGuard
                   <*> (lexeme (indented *> P.char '=') *> parseValue)

parseExternDeclaration :: P.Parsec String ParseState Declaration
parseExternDeclaration = P.try (reserved "foreign") *> indented *> reserved "import" *> indented *>
   (ExternDataDeclaration <$> (P.try (reserved "data") *> indented *> properName)
                             <*> (lexeme (indented *> P.string "::") *> parseKind)
   <|> do ident <- parseIdent
          js <- P.optionMaybe (JSRaw <$> stringLiteral)
          ty <- lexeme (indented *> P.string "::") *> parsePolyType
          return $ ExternDeclaration (if isJust js then InlineJavascript else ForeignImport) ident js ty)

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
  moduleName' <- moduleName
  idents <- P.optionMaybe $ parens $ commaSep1 parseDeclarationRef
  return $ ImportDeclaration moduleName' idents

parseDeclarationRef :: P.Parsec String ParseState DeclarationRef
parseDeclarationRef = ValueRef <$> parseIdent
                  <|> do name <- properName
                         dctors <- P.optionMaybe $ parens (Just <$> commaSep1 properName <|> lexeme (P.string "..") *> pure Nothing)
                         return $ maybe (TypeClassRef name) (TypeRef name) dctors

parseTypeClassDeclaration :: P.Parsec String ParseState Declaration
parseTypeClassDeclaration = do
  reserved "class"
  className <- indented *> properName
  idents <- P.many (indented *> identifier)
  members <- P.option [] . P.try $ do
    indented *> reserved "where"
    mark (P.many (same *> parseTypeDeclaration))
  return $ TypeClassDeclaration className idents members

parseTypeInstanceDeclaration :: P.Parsec String ParseState Declaration
parseTypeInstanceDeclaration = do
  reserved "instance"
  name <- parseIdent <* lexeme (indented *> P.string "::")
  deps <- P.optionMaybe $ do
    deps <- parens (commaSep1 ((,) <$> parseQualified properName <*> P.many parseTypeAtom))
    indented
    reservedOp "=>"
    return deps
  className <- indented *> parseQualified properName
  ty <- P.many (indented *> parseTypeAtom)
  members <- P.option [] . P.try $ do
    indented *> reserved "where"
    mark (P.many (same *> parseValueDeclaration))
  return $ TypeInstanceDeclaration name (fromMaybe [] deps) className ty members

-- |
-- Parse a single declaration
--
parseDeclaration :: P.Parsec String ParseState Declaration
parseDeclaration = P.choice
                   [ parseDataDeclaration
                   , parseTypeDeclaration
                   , parseTypeSynonymDeclaration
                   , parseValueDeclaration
                   , parseExternDeclaration
                   , parseFixityDeclaration
                   , parseImportDeclaration
                   , parseTypeClassDeclaration
                   , parseTypeInstanceDeclaration ] P.<?> "declaration"

-- |
-- Parse a module header and a collection of declarations
--
parseModule :: P.Parsec String ParseState Module
parseModule = do
  reserved "module"
  indented
  name <- moduleName
  exports <- P.optionMaybe $ parens $ commaSep1 parseDeclarationRef
  _ <- lexeme $ P.string "where"
  decls <- mark (P.many (same *> parseDeclaration))
  return $ Module name decls exports

-- |
-- Parse a collection of modules
--
parseModules :: P.Parsec String ParseState [Module]
parseModules = whiteSpace *> mark (P.many (same *> parseModule)) <* P.eof
