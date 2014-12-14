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

{-# LANGUAGE TupleSections #-}

module Language.PureScript.Parser.Declarations (
    parseDeclaration,
    parseModule,
    parseModules,
    parseModulesFromFiles,
    parseValue,
    parseGuard,
    parseBinder,
    parseBinderNoParens,
) where

import Data.Maybe (isJust, fromMaybe)
import Data.Traversable (forM)

import Control.Applicative
import Control.Arrow ((+++))

import Language.PureScript.Kinds
import Language.PureScript.Parser.State
import Language.PureScript.Parser.Common
import Language.PureScript.AST
import Language.PureScript.Parser.Types
import Language.PureScript.Parser.Kinds
import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.Environment

import qualified Language.PureScript.Parser.Common as C
import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as P

-- |
-- Read source position information
--
withSourceSpan :: (SourceSpan -> a -> a) -> P.Parsec s u a -> P.Parsec s u a
withSourceSpan f p = do
  start <- P.getPosition
  x <- p
  end <- P.getPosition
  let sp = SourceSpan (P.sourceName start) (toSourcePos start) (toSourcePos end)
  return $ f sp x
  where
  toSourcePos pos = SourcePos (P.sourceLine pos) (P.sourceColumn pos)

kindedIdent :: P.Parsec String ParseState (String, Maybe Kind)
kindedIdent = (, Nothing) <$> identifier
          <|> parens ((,) <$> identifier <*> (Just <$> (indented *> lexeme (P.string "::") *> indented *> parseKind)))

parseDataDeclaration :: P.Parsec String ParseState Declaration
parseDataDeclaration = do
  dtype <- (reserved "data" *> return Data) <|> (reserved "newtype" *> return Newtype)
  name <- indented *> properName
  tyArgs <- many (indented *> kindedIdent)
  ctors <- P.option [] $ do
    _ <- lexeme $ indented *> P.char '='
    sepBy1 ((,) <$> properName <*> P.many (indented *> noWildcards parseTypeAtom)) pipe
  return $ DataDeclaration dtype name tyArgs ctors

parseTypeDeclaration :: P.Parsec String ParseState Declaration
parseTypeDeclaration =
  TypeDeclaration <$> P.try (parseIdent <* lexeme (indented *> P.string "::"))
                  <*> parsePolyType

parseTypeSynonymDeclaration :: P.Parsec String ParseState Declaration
parseTypeSynonymDeclaration =
  TypeSynonymDeclaration <$> (P.try (reserved "type") *> indented *> properName)
                         <*> many (indented *> kindedIdent)
                         <*> (lexeme (indented *> P.char '=') *> noWildcards parsePolyType)

parseValueDeclaration :: P.Parsec String ParseState Declaration
parseValueDeclaration = do
  name <- parseIdent
  binders <- P.many parseBinderNoParens
  value <- Left <$> (C.indented *>
                       P.many1 ((,) <$> parseGuard
                                    <*> (lexeme (indented *> P.char '=') *> parseValueWithWhereClause)
                               ))
       <|> Right <$> (lexeme (indented *> P.char '=') *> parseValueWithWhereClause)
  return $ ValueDeclaration name Value binders value
  where
  parseValueWithWhereClause :: P.Parsec String ParseState Expr
  parseValueWithWhereClause = do
    value <- parseValue
    whereClause <- P.optionMaybe $ do
      C.indented
      reserved "where"
      C.indented
      C.mark $ P.many1 (C.same *> parseLocalDeclaration)
    return $ maybe value (`Let` value) whereClause

parseExternDeclaration :: P.Parsec String ParseState Declaration
parseExternDeclaration = P.try (reserved "foreign") *> indented *> reserved "import" *> indented *>
   (ExternDataDeclaration <$> (P.try (reserved "data") *> indented *> properName)
                          <*> (lexeme (indented *> P.string "::") *> parseKind)
   <|> (do reserved "instance"
           name <- parseIdent <* lexeme (indented *> P.string "::")
           deps <- P.option [] $ do
             deps <- parens (commaSep1 ((,) <$> parseQualified properName <*> P.many (noWildcards parseTypeAtom)))
             indented
             reservedOp "=>"
             return deps
           className <- indented *> parseQualified properName
           tys <- P.many (indented *> noWildcards parseTypeAtom)
           return $ ExternInstanceDeclaration name deps className tys)
   <|> (do ident <- parseIdent
           js <- P.optionMaybe (JSRaw <$> stringLiteral)
           ty <- lexeme (indented *> P.string "::") *> noWildcards parsePolyType
           return $ ExternDeclaration (if isJust js then InlineJavascript else ForeignImport) ident js ty))

parseAssociativity :: P.Parsec String ParseState Associativity
parseAssociativity =
  (P.try (reserved "infixl") >> return Infixl) <|>
  (P.try (reserved "infixr") >> return Infixr) <|>
  (P.try (reserved "infix") >>  return Infix)

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
  qualImport <|> stdImport
  where
  stdImport = do
    moduleName' <- moduleName
    stdImportHiding moduleName' <|> stdImportQualifying moduleName'
    where
    stdImportHiding mn = do
      reserved "hiding"
      declType <- importDeclarationType Hiding
      return $ ImportDeclaration mn declType Nothing
    stdImportQualifying mn = do
      declType <- importDeclarationType Qualifying
      return $ ImportDeclaration mn declType Nothing
  qualImport = do
    reserved "qualified"
    indented
    moduleName' <- moduleName
    declType <- importDeclarationType Qualifying
    reserved "as"
    asQ <- moduleName
    return $ ImportDeclaration moduleName' declType (Just asQ)
  importDeclarationType expectedType = do
    idents <- P.optionMaybe $ indented *> (parens $ commaSep parseDeclarationRef)
    return $ fromMaybe Unqualified (expectedType <$> idents)


parseDeclarationRef :: P.Parsec String ParseState DeclarationRef
parseDeclarationRef = withSourceSpan PositionedDeclarationRef $
  ValueRef <$> parseIdent
    <|> do name <- properName
           dctors <- P.optionMaybe $ parens (lexeme (P.string "..") *> pure Nothing <|> Just <$> commaSep properName)
           return $ maybe (TypeClassRef name) (TypeRef name) dctors

parseTypeClassDeclaration :: P.Parsec String ParseState Declaration
parseTypeClassDeclaration = do
  reserved "class"
  implies <- P.option [] $ do
    indented
    implies <- parens (commaSep1 ((,) <$> parseQualified properName <*> P.many (noWildcards parseTypeAtom)))
    reservedOp "<="
    return implies
  className <- indented *> properName
  idents <- P.many (indented *> kindedIdent)
  members <- P.option [] . P.try $ do
    indented *> reserved "where"
    mark (P.many (same *> positioned parseTypeDeclaration))
  return $ TypeClassDeclaration className idents implies members

parseTypeInstanceDeclaration :: P.Parsec String ParseState Declaration
parseTypeInstanceDeclaration = do
  reserved "instance"
  name <- parseIdent <* lexeme (indented *> P.string "::")
  deps <- P.optionMaybe $ do
    deps <- parens (commaSep1 ((,) <$> parseQualified properName <*> P.many (noWildcards parseTypeAtom)))
    indented
    reservedOp "=>"
    return deps
  className <- indented *> parseQualified properName
  ty <- P.many (indented *> (noWildcards parseTypeAtom))
  members <- P.option [] . P.try $ do
    indented *> reserved "where"
    mark (P.many (same *> positioned parseValueDeclaration))
  return $ TypeInstanceDeclaration name (fromMaybe [] deps) className ty members

positioned :: P.Parsec String ParseState Declaration -> P.Parsec String ParseState Declaration
positioned d = withSourceSpan PositionedDeclaration d

-- |
-- Parse a single declaration
--
parseDeclaration :: P.Parsec String ParseState Declaration
parseDeclaration = positioned (P.choice
                   [ parseDataDeclaration
                   , parseTypeDeclaration
                   , parseTypeSynonymDeclaration
                   , parseValueDeclaration
                   , parseExternDeclaration
                   , parseFixityDeclaration
                   , parseImportDeclaration
                   , parseTypeClassDeclaration
                   , parseTypeInstanceDeclaration
                   ]) P.<?> "declaration"

parseLocalDeclaration :: P.Parsec String ParseState Declaration
parseLocalDeclaration = positioned (P.choice
                   [ parseTypeDeclaration
                   , parseValueDeclaration
                   ] P.<?> "local declaration")

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
parseModulesFromFiles :: (k -> String) -> [(k, String)] -> Either P.ParseError [(k, Module)]
parseModulesFromFiles toFilePath input =
  fmap collect . forM input $ \(filename, content) -> do
    ms <- runIndentParser (toFilePath filename) parseModules content
    return (filename, ms)
  where
  collect :: [(k, [v])] -> [(k, v)]
  collect vss = [ (k, v) | (k, vs) <- vss, v <- vs ]

-- |
-- Parse a collection of modules
--
parseModules :: P.Parsec String ParseState [Module]
parseModules = whiteSpace *> mark (P.many (same *> parseModule)) <* P.eof

booleanLiteral :: P.Parsec String ParseState Bool
booleanLiteral = (C.reserved "true" >> return True) P.<|> (C.reserved "false" >> return False)

parseNumericLiteral :: P.Parsec String ParseState Expr
parseNumericLiteral = NumericLiteral <$> C.integerOrFloat

parseStringLiteral :: P.Parsec String ParseState Expr
parseStringLiteral = StringLiteral <$> C.stringLiteral

parseBooleanLiteral :: P.Parsec String ParseState Expr
parseBooleanLiteral = BooleanLiteral <$> booleanLiteral

parseArrayLiteral :: P.Parsec String ParseState Expr
parseArrayLiteral = ArrayLiteral <$> C.squares (C.commaSep parseValue)

parseObjectLiteral :: P.Parsec String ParseState Expr
parseObjectLiteral = ObjectLiteral <$> C.braces (C.commaSep parseIdentifierAndValue)

parseIdentifierAndValue :: P.Parsec String ParseState (String, Expr)
parseIdentifierAndValue = (,) <$> (C.indented *> (C.identifierName <|> C.stringLiteral) <* C.indented <* C.colon)
                              <*> (C.indented *> parseValue)

parseAbs :: P.Parsec String ParseState Expr
parseAbs = do
  C.reservedOp "\\"
  args <- P.many1 (C.indented *> (Abs <$> (Left <$> P.try C.parseIdent <|> Right <$> parseBinderNoParens)))
  C.indented *> C.reservedOp "->"
  value <- parseValue
  return $ toFunction args value
  where
  toFunction :: [Expr -> Expr] -> Expr -> Expr
  toFunction args value = foldr ($) value args

parseVar :: P.Parsec String ParseState Expr
parseVar = Var <$> C.parseQualified C.parseIdent

parseConstructor :: P.Parsec String ParseState Expr
parseConstructor = Constructor <$> C.parseQualified C.properName

parseCase :: P.Parsec String ParseState Expr
parseCase = Case <$> P.between (P.try (C.reserved "case")) (C.indented *> C.reserved "of") (return <$> parseValue)
                 <*> (C.indented *> C.mark (P.many (C.same *> C.mark parseCaseAlternative)))

parseCaseAlternative :: P.Parsec String ParseState CaseAlternative
parseCaseAlternative = CaseAlternative <$> (return <$> parseBinder)
                                       <*> (Left <$> (C.indented *>
                                                        P.many1 ((,) <$> parseGuard
                                                                     <*> (lexeme (indented *> C.reservedOp "->") *> parseValue)
                                                                ))
                                            <|> Right <$> (lexeme (indented *> C.reservedOp "->") *> parseValue))
                                       P.<?> "case alternative"

parseIfThenElse :: P.Parsec String ParseState Expr
parseIfThenElse = IfThenElse <$> (P.try (C.reserved "if") *> C.indented *> parseValue)
                             <*> (C.indented *> C.reserved "then" *> C.indented *> parseValue)
                             <*> (C.indented *> C.reserved "else" *> C.indented *> parseValue)

parseLet :: P.Parsec String ParseState Expr
parseLet = do
  C.reserved "let"
  C.indented
  ds <- C.mark $ P.many1 (C.same *> parseLocalDeclaration)
  C.indented
  C.reserved "in"
  result <- parseValue
  return $ Let ds result

parseValueAtom :: P.Parsec String ParseState Expr
parseValueAtom = P.choice
            [ P.try parseNumericLiteral
            , P.try parseStringLiteral
            , P.try parseBooleanLiteral
            , parseArrayLiteral
            , P.try parseObjectLiteral
            , parseAbs
            , P.try parseConstructor
            , P.try parseVar
            , parseCase
            , parseIfThenElse
            , parseDo
            , parseLet
            , Parens <$> C.parens parseValue ]

parsePropertyUpdate :: P.Parsec String ParseState (String, Expr)
parsePropertyUpdate = do
  name <- C.lexeme (C.identifierName <|> C.stringLiteral)
  _ <- C.lexeme $ C.indented *> P.char '='
  value <- C.indented *> parseValue
  return (name, value)

parseAccessor :: Expr -> P.Parsec String ParseState Expr
parseAccessor (Constructor _) = P.unexpected "constructor"
parseAccessor obj = P.try $ Accessor <$> (C.indented *> C.dot *> P.notFollowedBy C.opLetter *> C.indented *> (C.identifierName <|> C.stringLiteral)) <*> pure obj

parseDo :: P.Parsec String ParseState Expr
parseDo = do
  C.reserved "do"
  C.indented
  Do <$> C.mark (P.many (C.same *> C.mark parseDoNotationElement))

parseDoNotationLet :: P.Parsec String ParseState DoNotationElement
parseDoNotationLet = DoNotationLet <$> (C.reserved "let" *> C.indented *> C.mark (P.many1 (C.same *> parseLocalDeclaration)))

parseDoNotationBind :: P.Parsec String ParseState DoNotationElement
parseDoNotationBind = DoNotationBind <$> parseBinder <*> (C.indented *> C.reservedOp "<-" *> parseValue)

parseDoNotationElement :: P.Parsec String ParseState DoNotationElement
parseDoNotationElement = P.choice
            [ P.try parseDoNotationBind
            , parseDoNotationLet
            , P.try (DoNotationValue <$> parseValue) ]

-- |
-- Parse a value
--
parseValue :: P.Parsec String ParseState Expr
parseValue = withSourceSpan PositionedValue
  (P.buildExpressionParser operators
    . C.buildPostfixParser postfixTable2
    $ indexersAndAccessors) P.<?> "expression"
  where
  indexersAndAccessors = C.buildPostfixParser postfixTable1 parseValueAtom
  postfixTable1 = [ parseAccessor
                  , \v -> P.try $ flip ObjectUpdate <$> (C.indented *> C.braces (C.commaSep1 (C.indented *> parsePropertyUpdate))) <*> pure v ]
  postfixTable2 = [ \v -> P.try (flip App <$> (C.indented *> indexersAndAccessors)) <*> pure v
                  , \v -> flip (TypedValue True) <$> (P.try (C.lexeme (C.indented *> P.string "::")) *> parsePolyType) <*> pure v
                  ]
  operators = [ [ P.Prefix (C.lexeme (P.try (C.indented *> P.char '-') >> return UnaryMinus))
                ]
              , [ P.Infix (C.lexeme (P.try (C.indented *> C.parseIdentInfix P.<?> "operator") >>= \ident ->
                    return (BinaryNoParens ident))) P.AssocRight
                ]
              ]

parseStringBinder :: P.Parsec String ParseState Binder
parseStringBinder = StringBinder <$> C.stringLiteral

parseBooleanBinder :: P.Parsec String ParseState Binder
parseBooleanBinder = BooleanBinder <$> booleanLiteral

parseNumberBinder :: P.Parsec String ParseState Binder
parseNumberBinder = NumberBinder <$> (C.lexeme sign <*> C.integerOrFloat)
  where
  sign :: P.Parsec String ParseState (Either Integer Double -> Either Integer Double)
  sign = (P.char '-' >> return (negate +++ negate))
         <|> (P.char '+' >> return id)
         <|> return id

parseVarBinder :: P.Parsec String ParseState Binder
parseVarBinder = VarBinder <$> C.parseIdent

parseNullaryConstructorBinder :: P.Parsec String ParseState Binder
parseNullaryConstructorBinder = ConstructorBinder <$> C.lexeme (C.parseQualified C.properName) <*> pure []

parseConstructorBinder :: P.Parsec String ParseState Binder
parseConstructorBinder = ConstructorBinder <$> C.lexeme (C.parseQualified C.properName) <*> many (C.indented *> parseBinderNoParens)

parseObjectBinder :: P.Parsec String ParseState Binder
parseObjectBinder = ObjectBinder <$> C.braces (C.commaSep (C.indented *> parseIdentifierAndBinder))

parseArrayBinder :: P.Parsec String ParseState Binder
parseArrayBinder = C.squares $ ArrayBinder <$> C.commaSep (C.indented *> parseBinder)

parseNamedBinder :: P.Parsec String ParseState Binder
parseNamedBinder = NamedBinder <$> (C.parseIdent <* C.indented <* C.lexeme (P.char '@'))
                               <*> (C.indented *> parseBinder)

parseNullBinder :: P.Parsec String ParseState Binder
parseNullBinder = C.lexeme (P.char '_' *> P.notFollowedBy C.identLetter) *> return NullBinder

parseIdentifierAndBinder :: P.Parsec String ParseState (String, Binder)
parseIdentifierAndBinder = do
  name <- C.lexeme (C.identifierName <|> C.stringLiteral)
  _ <- C.lexeme $ C.indented *> (P.char '=' <|> P.char ':')
  binder <- C.indented *> parseBinder
  return (name, binder)

-- |
-- Parse a binder
--
parseBinder :: P.Parsec String ParseState Binder
parseBinder = withSourceSpan PositionedBinder (P.buildExpressionParser operators parseBinderAtom P.<?> "expression")
  where
  operators = [ [ P.Infix ( C.lexeme (P.try $ C.indented *> C.reservedOp ":") >> return ConsBinder) P.AssocRight ] ]
  parseBinderAtom :: P.Parsec String ParseState Binder
  parseBinderAtom = P.choice (map P.try
                    [ parseNullBinder
                    , parseStringBinder
                    , parseBooleanBinder
                    , parseNumberBinder
                    , parseNamedBinder
                    , parseVarBinder
                    , parseConstructorBinder
                    , parseObjectBinder
                    , parseArrayBinder
                    , C.parens parseBinder ]) P.<?> "binder"

-- |
-- Parse a binder as it would appear in a top level declaration
--
parseBinderNoParens :: P.Parsec String ParseState Binder
parseBinderNoParens = P.choice (map P.try
                  [ parseNullBinder
                  , parseStringBinder
                  , parseBooleanBinder
                  , parseNumberBinder
                  , parseNamedBinder
                  , parseVarBinder
                  , parseNullaryConstructorBinder
                  , parseObjectBinder
                  , parseArrayBinder
                  , C.parens parseBinder ]) P.<?> "binder"
-- |
-- Parse a guard
--
parseGuard :: P.Parsec String ParseState Guard
parseGuard = C.pipe *> C.indented *> parseValue


