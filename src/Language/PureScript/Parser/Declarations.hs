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
    parseModulesFromFiles,
    parseValue,
    parseGuard,
    parseBinder,
    parseBinderNoParens,
) where

import Prelude hiding (lex)

import Data.Maybe (isJust, fromMaybe)
import Data.Traversable (forM)

import Control.Applicative
import Control.Arrow ((+++))

import Language.PureScript.Kinds
import Language.PureScript.Parser.Common
import Language.PureScript.AST
import Language.PureScript.Parser.Types
import Language.PureScript.Parser.Kinds
import Language.PureScript.Parser.Indentation
import Language.PureScript.Parser.Lexer
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

kindedIdent :: TokenParser (String, Maybe Kind)
kindedIdent = (, Nothing) <$> identifier
          <|> parens ((,) <$> identifier <*> (Just <$> (doubleColon *> parseKind)))

parseDataDeclaration :: TokenParser Declaration
parseDataDeclaration = do
  dtype <- (reserved "data" *> return Data) <|> (reserved "newtype" *> return Newtype)
  name <- properName
  tyArgs <- many kindedIdent
  ctors <- P.option [] $ do
    equals
    P.sepBy1 ((,) <$> properName <*> P.many (noWildcards parseTypeAtom)) pipe
  return $ DataDeclaration dtype name tyArgs ctors

parseTypeDeclaration :: TokenParser Declaration
parseTypeDeclaration =
  TypeDeclaration <$> P.try (parseIdent <* doubleColon)
                  <*> parsePolyType

parseTypeSynonymDeclaration :: TokenParser Declaration
parseTypeSynonymDeclaration =
  TypeSynonymDeclaration <$> (P.try (reserved "type") *> properName)
                         <*> many kindedIdent
                         <*> (equals *> noWildcards parsePolyType)

parseValueDeclaration :: TokenParser Declaration
parseValueDeclaration = do
  name <- parseIdent
  binders <- P.many parseBinderNoParens
  value <- Left <$> (P.many1 ((,) <$> parseGuard
                                  <*> (equals *> parseValueWithWhereClause)
                              ))
       <|> Right <$> (equals *> parseValueWithWhereClause)
  return $ ValueDeclaration name Value binders value
  where
  parseValueWithWhereClause :: TokenParser Expr
  parseValueWithWhereClause = do
    value <- parseValue
    whereClause <- P.optionMaybe $ do
      reserved "where"
      block parseLocalDeclaration
    return $ maybe value (`Let` value) whereClause

parseExternDeclaration :: TokenParser Declaration
parseExternDeclaration = P.try (reserved "foreign") *> reserved "import" *>
   (ExternDataDeclaration <$> (P.try (reserved "data") *> properName)
                          <*> (doubleColon *> parseKind)
   <|> (do reserved "instance"
           name <- parseIdent <* doubleColon
           deps <- P.option [] $ do
             deps <- parens (commaSep1 ((,) <$> parseQualified properName <*> P.many (noWildcards parseTypeAtom)))
             rfatArrow
             return deps
           className <- parseQualified properName
           tys <- P.many (noWildcards parseTypeAtom)
           return $ ExternInstanceDeclaration name deps className tys)
   <|> (do ident <- parseIdent
           js <- P.optionMaybe (JSRaw <$> stringLiteral)
           ty <- doubleColon *> noWildcards parsePolyType
           return $ ExternDeclaration (if isJust js then InlineJavascript else ForeignImport) ident js ty))

parseAssociativity :: TokenParser Associativity
parseAssociativity =
  (P.try (reserved "infixl") >> return Infixl) <|>
  (P.try (reserved "infixr") >> return Infixr) <|>
  (P.try (reserved "infix") >>  return Infix)

parseFixity :: TokenParser Fixity
parseFixity = Fixity <$> parseAssociativity <*> natural

parseFixityDeclaration :: TokenParser Declaration
parseFixityDeclaration = FixityDeclaration <$> parseFixity <*> symbol

parseImportDeclaration :: TokenParser Declaration
parseImportDeclaration = do
  reserved "import"
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
    moduleName' <- moduleName
    declType <- importDeclarationType Qualifying
    reserved "as"
    asQ <- moduleName
    return $ ImportDeclaration moduleName' declType (Just asQ)
  importDeclarationType expectedType = do
    idents <- P.optionMaybe $ parens $ commaSep parseDeclarationRef
    return $ fromMaybe Unqualified (expectedType <$> idents)


parseDeclarationRef :: TokenParser DeclarationRef
parseDeclarationRef = withSourceSpan PositionedDeclarationRef $
  ValueRef <$> parseIdent
    <|> do name <- properName
           dctors <- P.optionMaybe $ parens (symbol' ".." *> pure Nothing <|> Just <$> commaSep properName)
           return $ maybe (TypeClassRef name) (TypeRef name) dctors

parseTypeClassDeclaration :: TokenParser Declaration
parseTypeClassDeclaration = do
  reserved "class"
  implies <- P.option [] $ do
    implies <- parens (commaSep1 ((,) <$> parseQualified properName <*> P.many (noWildcards parseTypeAtom)))
    lfatArrow
    return implies
  className <- properName
  idents <- P.many kindedIdent
  members <- P.option [] . P.try $ do
    reserved "where"
    block (positioned parseTypeDeclaration)
  return $ TypeClassDeclaration className idents implies members

parseTypeInstanceDeclaration :: TokenParser Declaration
parseTypeInstanceDeclaration = do
  reserved "instance"
  name <- parseIdent <* doubleColon
  deps <- P.optionMaybe $ do
    deps <- parens (commaSep1 ((,) <$> parseQualified properName <*> P.many (noWildcards parseTypeAtom)))
    rfatArrow
    return deps
  className <- parseQualified properName
  ty <- P.many (noWildcards parseTypeAtom)
  members <- P.option [] . P.try $ do
    reserved "where"
    block (positioned parseValueDeclaration)
  return $ TypeInstanceDeclaration name (fromMaybe [] deps) className ty members

positioned :: TokenParser Declaration -> TokenParser Declaration
positioned d = withSourceSpan PositionedDeclaration d

-- |
-- Parse a single declaration
--
parseDeclaration :: TokenParser Declaration
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

parseLocalDeclaration :: TokenParser Declaration
parseLocalDeclaration = positioned (P.choice
                   [ parseTypeDeclaration
                   , parseValueDeclaration
                   ] P.<?> "local declaration")

-- |
-- Parse a module header and a collection of declarations
--
parseModule :: TokenParser Module
parseModule = do
  reserved "module"
  name <- moduleName
  exports <- P.optionMaybe $ parens $ commaSep1 parseDeclarationRef
  _ <- reserved "where"
  decls <- genBlock True parseDeclaration
  return $ Module name decls exports

-- |
-- Parse a collection of modules
--
parseModulesFromFiles :: (k -> String) -> [(k, String)] -> Either P.ParseError [(k, Module)]
parseModulesFromFiles toFilePath input =
  forM input $ \(filename, content) -> do
    ts <- lex content
    ms <- runTokenParser (toFilePath filename) (parseModule <* P.eof) ts
    return (filename, ms)

booleanLiteral :: TokenParser Bool
booleanLiteral = (reserved "true" >> return True) P.<|> (reserved "false" >> return False)

parseNumericLiteral :: TokenParser Expr
parseNumericLiteral = NumericLiteral <$> number

parseStringLiteral :: TokenParser Expr
parseStringLiteral = StringLiteral <$> stringLiteral

parseBooleanLiteral :: TokenParser Expr
parseBooleanLiteral = BooleanLiteral <$> booleanLiteral

parseArrayLiteral :: TokenParser Expr
parseArrayLiteral = ArrayLiteral <$> squares (commaSep parseValue)

parseObjectLiteral :: TokenParser Expr
parseObjectLiteral = ObjectLiteral <$> braces (commaSep parseIdentifierAndValue)

parseIdentifierAndValue :: TokenParser (String, Expr)
parseIdentifierAndValue = (,) <$> ((lname <|> stringLiteral) <* colon)
                              <*> parseValue

parseAbs :: TokenParser Expr
parseAbs = do
  symbol' "\\"
  args <- P.many1 (Abs <$> (Left <$> P.try C.parseIdent <|> Right <$> parseBinderNoParens))
  rarrow
  value <- parseValue
  return $ toFunction args value
  where
  toFunction :: [Expr -> Expr] -> Expr -> Expr
  toFunction args value = foldr ($) value args

parseVar :: TokenParser Expr
parseVar = Var <$> C.parseQualified C.parseIdent

parseConstructor :: TokenParser Expr
parseConstructor = Constructor <$> C.parseQualified C.properName

parseCase :: TokenParser Expr
parseCase = Case <$> P.between (P.try (reserved "case")) (reserved "of") (return <$> parseValue)
                 <*> block parseCaseAlternative

parseCaseAlternative :: TokenParser CaseAlternative
parseCaseAlternative = CaseAlternative <$> (return <$> parseBinder)
                                       <*> (Left <$> P.many1 ((,) <$> parseGuard
                                                                   <*> (rarrow *> parseValue)
                                                              )
                                            <|> Right <$> (rarrow *> parseValue))
                                       P.<?> "case alternative"

parseIfThenElse :: TokenParser Expr
parseIfThenElse = IfThenElse <$> (P.try (reserved "if") *> parseValue)
                             <*> (reserved "then" *> parseValue)
                             <*> (reserved "else" *> parseValue)

parseLet :: TokenParser Expr
parseLet = do
  reserved "let"
  ds <- block parseLocalDeclaration
  reserved "in"
  result <- parseValue
  return $ Let ds result

parseValueAtom :: TokenParser Expr
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
            , Parens <$> parens parseValue ]

parsePropertyUpdate :: TokenParser (String, Expr)
parsePropertyUpdate = do
  name <- lname <|> stringLiteral
  equals
  value <- parseValue
  return (name, value)

parseAccessor :: Expr -> TokenParser Expr
parseAccessor (Constructor _) = P.unexpected "constructor"
parseAccessor obj = P.try $ Accessor <$> (dot *> (lname <|> stringLiteral)) <*> pure obj

parseDo :: TokenParser Expr
parseDo = do
  reserved "do"
  Do <$> block parseDoNotationElement

parseDoNotationLet :: TokenParser DoNotationElement
parseDoNotationLet = DoNotationLet <$> (reserved "let" *> block parseLocalDeclaration)

parseDoNotationBind :: TokenParser DoNotationElement
parseDoNotationBind = DoNotationBind <$> parseBinder <*> (larrow *> parseValue)

parseDoNotationElement :: TokenParser DoNotationElement
parseDoNotationElement = P.choice
            [ P.try parseDoNotationBind
            , P.try parseDoNotationLet
            , P.try (DoNotationValue <$> parseValue) ]

-- |
-- Parse a value
--
parseValue :: TokenParser Expr
parseValue = withSourceSpan PositionedValue
  (P.buildExpressionParser operators
    . C.buildPostfixParser postfixTable2
    $ indexersAndAccessors) P.<?> "expression"
  where
  indexersAndAccessors = C.buildPostfixParser postfixTable1 parseValueAtom
  postfixTable1 = [ parseAccessor
                  , \v -> P.try $ flip ObjectUpdate <$> (braces (commaSep1 parsePropertyUpdate)) <*> pure v ]
  postfixTable2 = [ \v -> P.try (flip App <$> indexersAndAccessors) <*> pure v
                  , \v -> flip (TypedValue True) <$> (P.try doubleColon *> parsePolyType) <*> pure v
                  ]
  operators = [ [ P.Prefix (P.try (symbol' "-") >> return UnaryMinus)
                ]
              , [ P.Infix (P.try (C.parseIdentInfix P.<?> "operator") >>= \ident ->
                    return (BinaryNoParens ident)) P.AssocRight
                ]
              ]

parseStringBinder :: TokenParser Binder
parseStringBinder = StringBinder <$> stringLiteral

parseBooleanBinder :: TokenParser Binder
parseBooleanBinder = BooleanBinder <$> booleanLiteral

parseNumberBinder :: TokenParser Binder
parseNumberBinder = NumberBinder <$> (sign <*> number)
  where
  sign :: TokenParser (Either Integer Double -> Either Integer Double)
  sign = (symbol' "-" >> return (negate +++ negate))
         <|> (symbol' "+" >> return id)
         <|> return id

parseVarBinder :: TokenParser Binder
parseVarBinder = VarBinder <$> C.parseIdent

parseNullaryConstructorBinder :: TokenParser Binder
parseNullaryConstructorBinder = ConstructorBinder <$> C.parseQualified C.properName <*> pure []

parseConstructorBinder :: TokenParser Binder
parseConstructorBinder = ConstructorBinder <$> C.parseQualified C.properName <*> many parseBinderNoParens

parseObjectBinder :: TokenParser Binder
parseObjectBinder = ObjectBinder <$> braces (commaSep parseIdentifierAndBinder)

parseArrayBinder :: TokenParser Binder
parseArrayBinder = squares $ ArrayBinder <$> commaSep parseBinder

parseNamedBinder :: TokenParser Binder
parseNamedBinder = NamedBinder <$> (C.parseIdent <* at)
                               <*> parseBinder

parseNullBinder :: TokenParser Binder
parseNullBinder = reserved "_" *> return NullBinder

parseIdentifierAndBinder :: TokenParser (String, Binder)
parseIdentifierAndBinder = do
  name <- lname <|> stringLiteral
  equals
  binder <- parseBinder
  return (name, binder)

-- |
-- Parse a binder
--
parseBinder :: TokenParser Binder
parseBinder = withSourceSpan PositionedBinder (P.buildExpressionParser operators parseBinderAtom P.<?> "expression")
  where
  operators = [ [ P.Infix (P.try $ colon *> return ConsBinder) P.AssocRight ] ]
  parseBinderAtom :: TokenParser Binder
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
                    , parens parseBinder ]) P.<?> "binder"

-- |
-- Parse a binder as it would appear in a top level declaration
--
parseBinderNoParens :: TokenParser Binder
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
                  , parens parseBinder ]) P.<?> "binder"
-- |
-- Parse a guard
--
parseGuard :: TokenParser Guard
parseGuard = pipe *> parseValue


