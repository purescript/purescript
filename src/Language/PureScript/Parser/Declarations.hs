-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Parser.Declarations
-- Copyright   :  (c) 2013-15 Phil Freeman, (c) 2014-15 Gary Burgess
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Parsers for module definitions and declarations
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    parseImportDeclaration',
    parseLocalDeclaration
) where

import Prelude hiding (lex)

import Data.Maybe (fromMaybe)

import Control.Applicative
import Control.Arrow ((+++))
import Control.Monad.Error.Class (MonadError(..))

import Language.PureScript.AST
import Language.PureScript.Comments
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Parser.Common
import Language.PureScript.Parser.Kinds
import Language.PureScript.Parser.Lexer
import Language.PureScript.Parser.Types

import qualified Language.PureScript.Parser.Common as C
import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as P

-- |
-- Read source position information
--
withSourceSpan :: (SourceSpan -> [Comment] -> a -> a) -> P.Parsec [PositionedToken] u a -> P.Parsec [PositionedToken] u a
withSourceSpan f p = do
  start <- P.getPosition
  comments <- C.readComments
  x <- p
  end <- P.getPosition
  let sp = SourceSpan (P.sourceName start) (toSourcePos start) (toSourcePos end)
  return $ f sp comments x

kindedIdent :: TokenParser (String, Maybe Kind)
kindedIdent = (, Nothing) <$> identifier
          <|> parens ((,) <$> identifier <*> (Just <$> (indented *> doubleColon *> indented *> parseKind)))

parseDataDeclaration :: TokenParser Declaration
parseDataDeclaration = do
  dtype <- (reserved "data" *> return Data) <|> (reserved "newtype" *> return Newtype)
  name <- indented *> properName
  tyArgs <- many (indented *> kindedIdent)
  ctors <- P.option [] $ do
    indented *> equals
    P.sepBy1 ((,) <$> properName <*> P.many (indented *> noWildcards parseTypeAtom)) pipe
  return $ DataDeclaration dtype name tyArgs ctors

parseTypeDeclaration :: TokenParser Declaration
parseTypeDeclaration =
  TypeDeclaration <$> P.try (parseIdent <* indented <* doubleColon)
                  <*> parsePolyType

parseTypeSynonymDeclaration :: TokenParser Declaration
parseTypeSynonymDeclaration =
  TypeSynonymDeclaration <$> (P.try (reserved "type") *> indented *> properName)
                         <*> many (indented *> kindedIdent)
                         <*> (indented *> equals *> noWildcards parsePolyType)

parseValueDeclaration :: TokenParser Declaration
parseValueDeclaration = do
  name <- parseIdent
  binders <- P.many parseBinderNoParens
  value <- Left <$> (C.indented *>
                       P.many1 ((,) <$> parseGuard
                                    <*> (indented *> equals *> parseValueWithWhereClause)
                               ))
       <|> Right <$> (indented *> equals *> parseValueWithWhereClause)
  return $ ValueDeclaration name Public binders value
  where
  parseValueWithWhereClause :: TokenParser Expr
  parseValueWithWhereClause = do
    value <- parseValue
    whereClause <- P.optionMaybe $ do
      C.indented
      reserved "where"
      C.indented
      C.mark $ P.many1 (C.same *> parseLocalDeclaration)
    return $ maybe value (`Let` value) whereClause

parseExternDeclaration :: TokenParser Declaration
parseExternDeclaration = P.try (reserved "foreign") *> indented *> reserved "import" *> indented *>
   (ExternDataDeclaration <$> (P.try (reserved "data") *> indented *> properName)
                          <*> (indented *> doubleColon *> parseKind)
   <|> (do ident <- parseIdent
           -- TODO: add a wiki page link with migration info
           -- TODO: remove this deprecation warning in 0.8
           _ <- P.optional $ stringLiteral *> featureWasRemoved "Inline foreign string literals are no longer supported."
           ty <- indented *> doubleColon *> noWildcards parsePolyType
           return $ ExternDeclaration ident ty))

parseAssociativity :: TokenParser Associativity
parseAssociativity =
  (P.try (reserved "infixl") >> return Infixl) <|>
  (P.try (reserved "infixr") >> return Infixr) <|>
  (P.try (reserved "infix") >>  return Infix)

parseFixity :: TokenParser Fixity
parseFixity = Fixity <$> parseAssociativity <*> (indented *> natural)

parseFixityDeclaration :: TokenParser Declaration
parseFixityDeclaration = do
  fixity <- parseFixity
  indented
  name <- symbol
  return $ FixityDeclaration fixity name

parseImportDeclaration :: TokenParser Declaration
parseImportDeclaration = do
  (mn, declType, asQ) <- parseImportDeclaration'
  return $ ImportDeclaration mn declType asQ

parseImportDeclaration' :: TokenParser (ModuleName, ImportDeclarationType, Maybe ModuleName)
parseImportDeclaration' = do
  reserved "import"
  indented
  qualImport <|> stdImport
  where
  stdImport = do
    moduleName' <- moduleName
    suffixHiding moduleName' <|> suffixQualifyingList moduleName'
    where
    suffixHiding mn = do
      reserved "hiding"
      declType <- qualifyingList Hiding
      return (mn, declType, Nothing)
    suffixQualifyingList mn = do
      declType <- qualifyingList Explicit
      qName <- P.optionMaybe qualifiedName
      return (mn, declType, qName)
  qualifiedName = reserved "as" *> moduleName
  qualImport = do
    reserved "qualified"
    indented
    moduleName' <- moduleName
    declType <- qualifyingList Explicit
    qName <- qualifiedName
    return (moduleName', declType, Just qName)
  qualifyingList expectedType = do
    idents <- P.optionMaybe $ indented *> parens (commaSep parseDeclarationRef)
    return $ fromMaybe Implicit (expectedType <$> idents)


parseDeclarationRef :: TokenParser DeclarationRef
parseDeclarationRef =
  parseModuleRef <|>
  withSourceSpan PositionedDeclarationRef
  (ValueRef <$> parseIdent
    <|> do name <- properName
           dctors <- P.optionMaybe $ parens (symbol' ".." *> pure Nothing <|> Just <$> commaSep properName)
           return $ maybe (TypeClassRef name) (TypeRef name) dctors
  )
  where
  parseModuleRef :: TokenParser DeclarationRef
  parseModuleRef = do
    name <- indented *> reserved "module" *> moduleName
    return $ ModuleRef name

parseTypeClassDeclaration :: TokenParser Declaration
parseTypeClassDeclaration = do
  reserved "class"
  implies <- P.option [] $ do
    indented
    implies <- parens (commaSep1 ((,) <$> parseQualified properName <*> P.many (noWildcards parseTypeAtom)))
    lfatArrow
    return implies
  className <- indented *> properName
  idents <- P.many (indented *> kindedIdent)
  members <- P.option [] . P.try $ do
    indented *> reserved "where"
    mark (P.many (same *> positioned parseTypeDeclaration))
  return $ TypeClassDeclaration className idents implies members

parseInstanceDeclaration :: TokenParser (TypeInstanceBody -> Declaration)
parseInstanceDeclaration = do
  reserved "instance"
  name <- parseIdent <* indented <* doubleColon
  deps <- P.optionMaybe $ do
    deps <- parens (commaSep1 ((,) <$> parseQualified properName <*> P.many (noWildcards parseTypeAtom)))
    indented
    rfatArrow
    return deps
  className <- indented *> parseQualified properName
  ty <- P.many (indented *> noWildcards parseTypeAtom)
  return $ TypeInstanceDeclaration name (fromMaybe [] deps) className ty

parseTypeInstanceDeclaration :: TokenParser Declaration
parseTypeInstanceDeclaration = do
  instanceDecl <- parseInstanceDeclaration
  members <- P.option [] . P.try $ do
    indented *> reserved "where"
    mark (P.many (same *> positioned parseValueDeclaration))
  return $ instanceDecl (ExplicitInstance members)

parseDerivingInstanceDeclaration :: TokenParser Declaration
parseDerivingInstanceDeclaration = do
  reserved "derive"
  instanceDecl <- parseInstanceDeclaration
  return $ instanceDecl DerivedInstance

positioned :: TokenParser Declaration -> TokenParser Declaration
positioned = withSourceSpan PositionedDeclaration

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
                   , parseDerivingInstanceDeclaration
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
  comments <- C.readComments
  start <- P.getPosition
  reserved "module"
  indented
  name <- moduleName
  exports <- P.optionMaybe $ parens $ commaSep1 parseDeclarationRef
  reserved "where"
  decls <- mark (P.many (same *> parseDeclaration))
  end <- P.getPosition
  let ss = SourceSpan (P.sourceName start) (toSourcePos start) (toSourcePos end)
  return $ Module ss comments name decls exports

-- |
-- Parse a collection of modules
--
parseModulesFromFiles :: forall m k. (MonadError MultipleErrors m, Functor m) =>
                                     (k -> FilePath) -> [(k, String)] -> m [(k, Module)]
parseModulesFromFiles toFilePath input = do
  modules <- parU input $ \(k, content) -> do
    let filename = toFilePath k
    ts <- wrapError $ lex filename content
    ms <- wrapError $ runTokenParser filename parseModules ts
    return (k, ms)
  return $ collect modules
  where
  wrapError :: Either P.ParseError a -> m a
  wrapError = either (throwError . MultipleErrors . pure . toPositionedError) return
  collect :: [(k, [v])] -> [(k, v)]
  collect vss = [ (k, v) | (k, vs) <- vss, v <- vs ]

toPositionedError :: P.ParseError -> ErrorMessage
toPositionedError perr = ErrorMessage [ PositionedError (SourceSpan name start end) ] (ErrorParsingModule perr)
  where
  name   = (P.sourceName . P.errorPos) perr
  start  = (toSourcePos  . P.errorPos) perr
  end    = start

toSourcePos :: P.SourcePos -> SourcePos
toSourcePos pos = SourcePos (P.sourceLine pos) (P.sourceColumn pos)

-- |
-- Parse a collection of modules
--
parseModules :: TokenParser [Module]
parseModules = mark (P.many (same *> parseModule)) <* P.eof

booleanLiteral :: TokenParser Bool
booleanLiteral = (reserved "true" >> return True) P.<|> (reserved "false" >> return False)

parseNumericLiteral :: TokenParser Expr
parseNumericLiteral = NumericLiteral <$> number

parseCharLiteral :: TokenParser Expr
parseCharLiteral = CharLiteral <$> charLiteral

parseStringLiteral :: TokenParser Expr
parseStringLiteral = StringLiteral <$> stringLiteral

parseBooleanLiteral :: TokenParser Expr
parseBooleanLiteral = BooleanLiteral <$> booleanLiteral

parseArrayLiteral :: TokenParser Expr
parseArrayLiteral = ArrayLiteral <$> squares (commaSep parseValue)

parseObjectLiteral :: TokenParser Expr
parseObjectLiteral = ObjectConstructor <$> braces (commaSep parseIdentifierAndValue)

parseIdentifierAndValue :: TokenParser (String, Maybe Expr)
parseIdentifierAndValue = (,) <$> (C.indented *> (lname <|> stringLiteral) <* C.indented <* colon)
                              <*> (C.indented *> val)
  where
  val = (Just <$> parseValue) <|> (underscore *> pure Nothing)

parseAbs :: TokenParser Expr
parseAbs = do
  symbol' "\\"
  args <- P.many1 (C.indented *> (Abs <$> (Left <$> P.try C.parseIdent <|> Right <$> parseBinderNoParens)))
  C.indented *> rarrow
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
parseCase = Case <$> P.between (P.try (reserved "case")) (C.indented *> reserved "of") (return <$> parseValue)
                 <*> (C.indented *> C.mark (P.many1 (C.same *> C.mark parseCaseAlternative)))

parseCaseAlternative :: TokenParser CaseAlternative
parseCaseAlternative = CaseAlternative <$> (return <$> parseBinder)
                                       <*> (Left <$> (C.indented *>
                                                        P.many1 ((,) <$> parseGuard
                                                                     <*> (indented *> rarrow *> parseValue)
                                                                ))
                                            <|> Right <$> (indented *> rarrow *> parseValue))
                                       P.<?> "case alternative"

parseIfThenElse :: TokenParser Expr
parseIfThenElse = IfThenElse <$> (P.try (reserved "if") *> C.indented *> parseValue)
                             <*> (C.indented *> reserved "then" *> C.indented *> parseValue)
                             <*> (C.indented *> reserved "else" *> C.indented *> parseValue)

parseLet :: TokenParser Expr
parseLet = do
  reserved "let"
  C.indented
  ds <- C.mark $ P.many1 (C.same *> parseLocalDeclaration)
  C.indented
  reserved "in"
  result <- parseValue
  return $ Let ds result

parseValueAtom :: TokenParser Expr
parseValueAtom = P.choice
            [ P.try parseNumericLiteral
            , P.try parseCharLiteral
            , P.try parseStringLiteral
            , P.try parseBooleanLiteral
            , parseArrayLiteral
            , P.try parseObjectLiteral
            , P.try parseObjectGetter
            , parseAbs
            , P.try parseConstructor
            , P.try parseVar
            , parseCase
            , parseIfThenElse
            , parseDo
            , parseLet
            , P.try $ Parens <$> parens parseValue
            , parseOperatorSection
            , P.try parseObjectUpdaterWildcard ]

-- |
-- Parse an expression in backticks or an operator
--
parseInfixExpr :: TokenParser Expr
parseInfixExpr = P.between tick tick parseValue
                 <|> Var <$> parseQualified (Op <$> symbol)

parseOperatorSection :: TokenParser Expr
parseOperatorSection = parens $ left <|> right
  where
  right = OperatorSection <$> parseInfixExpr <* indented <*> (Right <$> parseValueAtom)
  left = flip OperatorSection <$> (Left <$> parseValueAtom) <* indented <*> parseInfixExpr

parsePropertyUpdate :: TokenParser (String, Maybe Expr)
parsePropertyUpdate = do
  name <- lname <|> stringLiteral
  _ <- C.indented *> equals
  value <- C.indented *> (underscore *> pure Nothing) <|> (Just <$> parseValue)
  return (name, value)

parseAccessor :: Expr -> TokenParser Expr
parseAccessor (Constructor _) = P.unexpected "constructor"
parseAccessor obj = P.try $ Accessor <$> (C.indented *> dot *> C.indented *> (lname <|> stringLiteral)) <*> pure obj

parseDo :: TokenParser Expr
parseDo = do
  reserved "do"
  C.indented
  Do <$> C.mark (P.many1 (C.same *> C.mark parseDoNotationElement))

parseDoNotationLet :: TokenParser DoNotationElement
parseDoNotationLet = DoNotationLet <$> (reserved "let" *> C.indented *> C.mark (P.many1 (C.same *> parseLocalDeclaration)))

parseDoNotationBind :: TokenParser DoNotationElement
parseDoNotationBind = DoNotationBind <$> parseBinder <*> (C.indented *> larrow *> parseValue)

parseDoNotationElement :: TokenParser DoNotationElement
parseDoNotationElement = P.choice
            [ P.try parseDoNotationBind
            , parseDoNotationLet
            , P.try (DoNotationValue <$> parseValue) ]

parseObjectGetter :: TokenParser Expr
parseObjectGetter = ObjectGetter <$> (underscore *> C.indented *> dot *> C.indented *> (lname <|> stringLiteral))

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
                  , P.try . parseUpdaterBody . Just ]
  postfixTable2 = [ \v -> P.try (flip App <$> (C.indented *> indexersAndAccessors)) <*> pure v
                  , \v -> flip (TypedValue True) <$> (P.try (C.indented *> doubleColon) *> parsePolyType) <*> pure v
                  ]
  operators = [ [ P.Prefix (P.try (C.indented *> symbol' "-") >> return UnaryMinus)
                ]
              , [ P.Infix (P.try (C.indented *> parseInfixExpr P.<?> "infix expression") >>= \ident ->
                    return (BinaryNoParens ident)) P.AssocRight
                ]
              ]

parseUpdaterBody :: Maybe Expr -> TokenParser Expr
parseUpdaterBody v = ObjectUpdater v <$> (C.indented *> braces (commaSep1 (C.indented *> parsePropertyUpdate)))

parseObjectUpdaterWildcard :: TokenParser Expr
parseObjectUpdaterWildcard = underscore *> C.indented *> parseUpdaterBody Nothing

parseStringBinder :: TokenParser Binder
parseStringBinder = StringBinder <$> stringLiteral

parseCharBinder :: TokenParser Binder
parseCharBinder = CharBinder <$> charLiteral

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
parseConstructorBinder = ConstructorBinder <$> C.parseQualified C.properName <*> many (C.indented *> parseBinderNoParens)

parseObjectBinder :: TokenParser Binder
parseObjectBinder = ObjectBinder <$> braces (commaSep (C.indented *> parseIdentifierAndBinder))

parseArrayBinder :: TokenParser Binder
parseArrayBinder = squares $ ArrayBinder <$> commaSep (C.indented *> parseBinder)

parseNamedBinder :: TokenParser Binder
parseNamedBinder = NamedBinder <$> (C.parseIdent <* C.indented <* at)
                               <*> (C.indented *> parseBinder)

parseNullBinder :: TokenParser Binder
parseNullBinder = underscore *> return NullBinder

parseIdentifierAndBinder :: TokenParser (String, Binder)
parseIdentifierAndBinder = do
  name <- lname <|> stringLiteral
  C.indented *> (equals <|> colon)
  binder <- C.indented *> parseBinder
  return (name, binder)

-- |
-- Parse a binder
--
parseBinder :: TokenParser Binder
parseBinder = withSourceSpan PositionedBinder (P.buildExpressionParser operators (buildPostfixParser postfixTable parseBinderAtom))
  where
  -- TODO: remove this deprecation warning in 0.8
  operators = [ [ P.Infix (P.try $ C.indented *> colon *> featureWasRemoved "Cons binders are no longer supported. Consider using purescript-lists or purescript-sequences instead.") P.AssocRight ] ]
  -- TODO: parsePolyType when adding support for polymorphic types
  postfixTable = [ \b -> flip TypedBinder b <$> (P.try (indented *> doubleColon) *> parseType)
                 ]
  parseBinderAtom :: TokenParser Binder
  parseBinderAtom = P.choice (map P.try
                    [ parseNullBinder
                    , parseCharBinder
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
                  , parseCharBinder
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
parseGuard = pipe *> C.indented *> parseValue
