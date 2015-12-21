{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Parsers for module definitions and declarations
--
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
import Control.Parallel.Strategies (withStrategy, parList, rseq)

import Language.PureScript.AST
import Language.PureScript.Comments
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Types
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
  TypeSynonymDeclaration <$> (reserved "type" *> indented *> properName)
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
parseExternDeclaration = reserved "foreign" *> indented *> reserved "import" *> indented *>
   (ExternDataDeclaration <$> (reserved "data" *> indented *> properName)
                          <*> (indented *> doubleColon *> parseKind)
   <|> (do ident <- parseIdent
           ty <- indented *> doubleColon *> noWildcards parsePolyType
           return $ ExternDeclaration ident ty))

parseAssociativity :: TokenParser Associativity
parseAssociativity =
  (reserved "infixl" *> return Infixl) <|>
  (reserved "infixr" *> return Infixr) <|>
  (reserved "infix"  *> return Infix)

parseFixity :: TokenParser Fixity
parseFixity = Fixity <$> parseAssociativity <*> (indented *> natural)

parseFixityDeclaration :: TokenParser Declaration
parseFixityDeclaration = do
  fixity <- parseFixity
  indented
  alias <- P.optionMaybe $ parseQualified (Ident <$> identifier) <* reserved "as"
  name <- symbol
  return $ FixityDeclaration fixity name alias

parseImportDeclaration :: TokenParser Declaration
parseImportDeclaration = do
  (mn, declType, asQ, isOldSyntax) <- parseImportDeclaration'
  return $ ImportDeclaration mn declType asQ isOldSyntax

parseImportDeclaration' :: TokenParser (ModuleName, ImportDeclarationType, Maybe ModuleName, Bool)
parseImportDeclaration' = do
  reserved "import"
  indented
  qualImport <|> stdImport
  where
  stdImport = do
    moduleName' <- moduleName
    declType <- reserved "hiding" *> qualifyingList Hiding <|> qualifyingList Explicit
    qName <- P.optionMaybe qualifiedName
    return (moduleName', declType, qName, False)
  qualifiedName = reserved "as" *> moduleName
  qualImport = do
    reserved "qualified"
    indented
    moduleName' <- moduleName
    declType <- qualifyingList Explicit
    qName <- qualifiedName
    return (moduleName', declType, Just qName, True)
  qualifyingList expectedType = do
    declType <- P.optionMaybe (expectedType <$> (indented *> parens (commaSep parseDeclarationRef)))
    return $ fromMaybe Implicit declType

parseDeclarationRef :: TokenParser DeclarationRef
parseDeclarationRef =
  withSourceSpan PositionedDeclarationRef
    $ (ValueRef <$> parseIdent)
    <|> parseProperRef
    <|> (TypeClassRef <$> (reserved "class" *> properName))
    <|> (ModuleRef <$> (indented *> reserved "module" *> moduleName))
  where
  parseProperRef = do
    name <- properName
    dctors <- P.optionMaybe $ parens (symbol' ".." *> pure Nothing <|> Just <$> commaSep properName)
    return $ maybe (ProperRef name) (TypeRef name) dctors

parseTypeClassDeclaration :: TokenParser Declaration
parseTypeClassDeclaration = do
  reserved "class"
  implies <- P.option [] . P.try $ do
    indented
    implies <- (return <$> parseConstraint) <|> parens (commaSep1 parseConstraint)
    lfatArrow
    return implies
  className <- indented *> properName
  idents <- P.many (indented *> kindedIdent)
  members <- P.option [] . P.try $ do
    indented *> reserved "where"
    indented *> mark (P.many (same *> positioned parseTypeDeclaration))
  return $ TypeClassDeclaration className idents implies members
  where

parseConstraint :: TokenParser Constraint
parseConstraint = (,) <$> parseQualified properName <*> P.many (noWildcards parseTypeAtom)

parseInstanceDeclaration :: TokenParser (TypeInstanceBody -> Declaration)
parseInstanceDeclaration = do
  reserved "instance"
  name <- parseIdent <* indented <* doubleColon
  deps <- P.optionMaybe $ P.try $ do
    deps <- (return <$> parseConstraint) <|> parens (commaSep1 parseConstraint)
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

-- | Parse a collection of modules in parallel
parseModulesFromFiles :: forall m k. (MonadError MultipleErrors m, Functor m) =>
                                     (k -> FilePath) -> [(k, String)] -> m [(k, Module)]
parseModulesFromFiles toFilePath input = do
  modules <- flip parU id $ map wrapError $ inParallel $ flip map input $ \(k, content) -> do
    let filename = toFilePath k
    ts <- lex filename content
    ms <- runTokenParser filename parseModules ts
    return (k, ms)
  return $ collect modules
  where
  collect :: [(k, [v])] -> [(k, v)]
  collect vss = [ (k, v) | (k, vs) <- vss, v <- vs ]
  wrapError :: Either P.ParseError a -> m a
  wrapError = either (throwError . MultipleErrors . pure . toPositionedError) return
  -- It is enough to force each parse result to WHNF, since success or failure can't be
  -- determined until the end of the file, so this effectively distributes parsing of each file
  -- to a different spark.
  inParallel :: [Either P.ParseError (k, [Module])] -> [Either P.ParseError (k, [Module])]
  inParallel = withStrategy (parList rseq)

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
parseIdentifierAndValue =
  do
    name <- C.indented *> lname
    b <- P.option (Just $ Var $ Qualified Nothing (Ident name)) rest
    return (name, b)
  <|> (,) <$> (C.indented *> stringLiteral) <*> rest
  where
  rest = C.indented *> colon *> C.indented *> val
  val = P.try (Just <$> parseValue) <|> (underscore *> pure Nothing)

parseAbs :: TokenParser Expr
parseAbs = do
  symbol' "\\"
  -- TODO: remove this 'try' after operator aliases are finished (0.9)
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
parseCase = Case <$> P.between (reserved "case") (C.indented *> reserved "of") (commaSep1 parseValue)
                 <*> (C.indented *> C.mark (P.many1 (C.same *> C.mark parseCaseAlternative)))

parseCaseAlternative :: TokenParser CaseAlternative
parseCaseAlternative = CaseAlternative <$> (commaSep1 parseBinder)
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
                 [ parseNumericLiteral
                 , parseCharLiteral
                 , parseStringLiteral
                 , parseBooleanLiteral
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
                 -- TODO: combine this with parseObjectGetter
                 , parseObjectUpdaterWildcard
                 ]

-- |
-- Parse an expression in backticks or an operator
--
parseInfixExpr :: TokenParser Expr
parseInfixExpr = P.between tick tick parseValue
                 <|> Var <$> parseQualified (Op <$> symbol)

parseOperatorSection :: TokenParser Expr
parseOperatorSection = parens $ left <|> right
  where
  right = OperatorSection <$> parseInfixExpr <* indented <*> (Right <$> indexersAndAccessors)
  left = flip OperatorSection <$> (Left <$> indexersAndAccessors) <* indented <*> parseInfixExpr

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
parseDoNotationBind = DoNotationBind <$> P.try (parseBinder <* C.indented <* larrow) <*> parseValue

parseDoNotationElement :: TokenParser DoNotationElement
parseDoNotationElement = P.choice
            [ parseDoNotationBind
            , parseDoNotationLet
            , DoNotationValue <$> parseValue
            ]

parseObjectGetter :: TokenParser Expr
parseObjectGetter = ObjectGetter <$> (underscore *> C.indented *> dot *> C.indented *> (lname <|> stringLiteral))

-- | Expressions including indexers and record updates
indexersAndAccessors :: TokenParser Expr
indexersAndAccessors = C.buildPostfixParser postfixTable parseValueAtom
  where
  postfixTable = [ parseAccessor
                 , P.try . parseUpdaterBody . Just
                 ]

-- |
-- Parse a value
--
parseValue :: TokenParser Expr
parseValue = withSourceSpan PositionedValue
  (P.buildExpressionParser operators
    . C.buildPostfixParser postfixTable
    $ indexersAndAccessors) P.<?> "expression"
  where
  postfixTable = [ \v -> P.try (flip App <$> (C.indented *> indexersAndAccessors)) <*> pure v
                 , \v -> flip (TypedValue True) <$> (C.indented *> doubleColon *> parsePolyType) <*> pure v
                 ]
  operators = [ [ P.Prefix (C.indented *> symbol' "-" *> return UnaryMinus)
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

parseNullaryConstructorBinder :: TokenParser Binder
parseNullaryConstructorBinder = ConstructorBinder <$> C.parseQualified C.properName <*> pure []

parseConstructorBinder :: TokenParser Binder
parseConstructorBinder = ConstructorBinder <$> C.parseQualified C.properName <*> many (C.indented *> parseBinderNoParens)

parseObjectBinder :: TokenParser Binder
parseObjectBinder = ObjectBinder <$> braces (commaSep (C.indented *> parseIdentifierAndBinder))

parseArrayBinder :: TokenParser Binder
parseArrayBinder = squares $ ArrayBinder <$> commaSep (C.indented *> parseBinder)

parseVarOrNamedBinder :: TokenParser Binder
parseVarOrNamedBinder = do
  -- TODO: once operator aliases are finalized in 0.9, this 'try' won't be needed
  -- any more since identifiers in binders won't be 'Op's.
  name <- P.try C.parseIdent
  let parseNamedBinder = NamedBinder name <$> (at *> C.indented *> parseBinder)
  parseNamedBinder <|> return (VarBinder name)

parseNullBinder :: TokenParser Binder
parseNullBinder = underscore *> return NullBinder

parseIdentifierAndBinder :: TokenParser (String, Binder)
parseIdentifierAndBinder =
  do name <- lname
     b <- P.option (VarBinder (Ident name)) rest
     return (name, b)
  <|> (,) <$> stringLiteral <*> rest
  where
  rest = C.indented *> (equals <|> colon) *> C.indented *> parseBinder

-- |
-- Parse a binder
--
parseBinder :: TokenParser Binder
parseBinder = withSourceSpan PositionedBinder (buildPostfixParser postfixTable parseBinderAtom)
  where
  -- TODO: parsePolyType when adding support for polymorphic types
  postfixTable = [ \b -> flip TypedBinder b <$> (indented *> doubleColon *> parseType)
                 ]
  parseBinderAtom :: TokenParser Binder
  parseBinderAtom = P.choice
                    [ parseNullBinder
                    , parseCharBinder
                    , parseStringBinder
                    , parseBooleanBinder
                    , parseNumberBinder
                    , parseVarOrNamedBinder
                    , parseConstructorBinder
                    , parseObjectBinder
                    , parseArrayBinder
                    , parens parseBinder
                    ] P.<?> "binder"

-- |
-- Parse a binder as it would appear in a top level declaration
--
parseBinderNoParens :: TokenParser Binder
parseBinderNoParens = P.choice
                      [ parseNullBinder
                      , parseCharBinder
                      , parseStringBinder
                      , parseBooleanBinder
                      , parseNumberBinder
                      , parseVarOrNamedBinder
                      , parseNullaryConstructorBinder
                      , parseObjectBinder
                      , parseArrayBinder
                      , parens parseBinder
                      ] P.<?> "binder"

-- |
-- Parse a guard
--
parseGuard :: TokenParser Guard
parseGuard = pipe *> C.indented *> parseValue
