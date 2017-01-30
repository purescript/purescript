-- | Parsers for module definitions and declarations
module Language.PureScript.Parser.Declarations
  ( parseDeclaration
  , parseModule
  , parseModulesFromFiles
  , parseModuleFromFile
  , parseValue
  , parseGuard
  , parseBinder
  , parseBinderNoParens
  , parseImportDeclaration'
  , parseLocalDeclaration
  , toPositionedError
  ) where

import           Prelude hiding (lex)

import           Control.Applicative
import           Control.Arrow ((+++))
import           Control.Monad (foldM)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Parallel.Strategies (withStrategy, parList, rseq)
import           Data.Functor (($>))
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import           Data.Text (Text)
import           Language.PureScript.AST
import           Language.PureScript.Environment
import           Language.PureScript.Errors
import           Language.PureScript.Kinds
import           Language.PureScript.Names
import           Language.PureScript.Parser.Common
import           Language.PureScript.Parser.Kinds
import           Language.PureScript.Parser.Lexer
import           Language.PureScript.Parser.Types
import           Language.PureScript.PSString (PSString, mkString)
import           Language.PureScript.Types
import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as P

kindedIdent :: TokenParser (Text, Maybe Kind)
kindedIdent = (, Nothing) <$> identifier
          <|> parens ((,) <$> identifier <*> (Just <$> (indented *> doubleColon *> indented *> parseKind)))

parseDataDeclaration :: TokenParser Declaration
parseDataDeclaration = do
  dtype <- (reserved "data" *> return Data) <|> (reserved "newtype" *> return Newtype)
  name <- indented *> typeName
  tyArgs <- many (indented *> kindedIdent)
  ctors <- P.option [] $ do
    indented *> equals
    P.sepBy1 ((,) <$> dataConstructorName <*> P.many (indented *> noWildcards parseTypeAtom)) pipe
  return $ DataDeclaration dtype name tyArgs ctors

parseTypeDeclaration :: TokenParser Declaration
parseTypeDeclaration =
  TypeDeclaration <$> P.try (parseIdent <* indented <* doubleColon)
                  <*> parsePolyType

parseTypeSynonymDeclaration :: TokenParser Declaration
parseTypeSynonymDeclaration =
  TypeSynonymDeclaration <$> (reserved "type" *> indented *> typeName)
                         <*> many (indented *> kindedIdent)
                         <*> (indented *> equals *> noWildcards parsePolyType)

parseValueDeclaration :: TokenParser Declaration
parseValueDeclaration = do
  name <- parseIdent
  binders <- P.many parseBinderNoParens
  value <- Left <$> (indented *>
                       P.many1 ((,) <$> parseGuard
                                    <*> (indented *> equals *> parseValueWithWhereClause)
                               ))
       <|> Right <$> (indented *> equals *> parseValueWithWhereClause)
  return $ ValueDeclaration name Public binders value
  where
  parseValueWithWhereClause :: TokenParser Expr
  parseValueWithWhereClause = do
    indented
    value <- parseValue
    whereClause <- P.optionMaybe $ do
      indented
      reserved "where"
      indented
      mark $ P.many1 (same *> parseLocalDeclaration)
    return $ maybe value (`Let` value) whereClause

parseExternDeclaration :: TokenParser Declaration
parseExternDeclaration = reserved "foreign" *> indented *> reserved "import" *> indented *> parseExternAlt where
  parseExternAlt = parseExternData <|> P.try parseExternKind <|> parseExternTerm

  parseExternData = ExternDataDeclaration <$> (reserved "data" *> indented *> typeName)
                                          <*> (indented *> doubleColon *> parseKind)

  parseExternKind = ExternKindDeclaration <$> (reserved "kind" *> indented *> kindName)

  parseExternTerm = ExternDeclaration <$> parseIdent
                                      <*> (indented *> doubleColon *> noWildcards parsePolyType)

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
  FixityDeclaration
    <$> ((Right <$> typeFixity fixity) <|> (Left <$> valueFixity fixity))
  where
  typeFixity fixity =
    TypeFixity fixity
      <$> (reserved "type" *> parseQualified typeName)
      <*> (reserved "as" *> parseOperator)
  valueFixity fixity =
    ValueFixity fixity
      <$> parseQualified ((Left <$> parseIdent) <|> (Right <$> properName))
      <*> (reserved "as" *> parseOperator)

parseImportDeclaration :: TokenParser Declaration
parseImportDeclaration = withSourceSpan PositionedDeclaration $ do
  (mn, declType, asQ) <- parseImportDeclaration'
  return $ ImportDeclaration mn declType asQ

parseImportDeclaration' :: TokenParser (ModuleName, ImportDeclarationType, Maybe ModuleName)
parseImportDeclaration' = do
  reserved "import"
  indented
  moduleName' <- moduleName
  declType <- reserved "hiding" *> qualifyingList Hiding <|> qualifyingList Explicit
  qName <- P.optionMaybe qualifiedName
  return (moduleName', declType, qName)
  where
  qualifiedName = reserved "as" *> moduleName
  qualifyingList expectedType = do
    declType <- P.optionMaybe (expectedType <$> (indented *> parens (commaSep parseDeclarationRef)))
    return $ fromMaybe Implicit declType

parseDeclarationRef :: TokenParser DeclarationRef
parseDeclarationRef =
  withSourceSpan PositionedDeclarationRef
    $ (KindRef <$> P.try (reserved "kind" *> kindName))
    <|> (ValueRef <$> parseIdent)
    <|> (ValueOpRef <$> parens parseOperator)
    <|> parseTypeRef
    <|> (TypeClassRef <$> (reserved "class" *> properName))
    <|> (ModuleRef <$> (indented *> reserved "module" *> moduleName))
    <|> (TypeOpRef <$> (indented *> reserved "type" *> parens parseOperator))
  where
  parseTypeRef = do
    name <- typeName
    dctors <- P.optionMaybe $ parens (symbol' ".." *> pure Nothing <|> Just <$> commaSep properName)
    return $ TypeRef name (fromMaybe (Just []) dctors)

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
  let parseNamedIdent = foldl (<|>) empty (zipWith (\(name, _) index -> lname' name $> index) idents [0..])
      parseFunctionalDependency =
        FunctionalDependency <$> P.many parseNamedIdent <* rarrow
                             <*> P.many parseNamedIdent
  dependencies <- P.option [] (indented *> pipe *> commaSep1 parseFunctionalDependency)
  members <- P.option [] $ do
    indented *> reserved "where"
    indented *> mark (P.many (same *> positioned parseTypeDeclaration))
  return $ TypeClassDeclaration className idents implies dependencies members

parseConstraint :: TokenParser Constraint
parseConstraint = Constraint <$> parseQualified properName
                             <*> P.many (noWildcards parseTypeAtom)
                             <*> pure Nothing

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
  ty <- P.many (indented *> parseTypeAtom)
  return $ TypeInstanceDeclaration name (fromMaybe [] deps) className ty

parseTypeInstanceDeclaration :: TokenParser Declaration
parseTypeInstanceDeclaration = do
  instanceDecl <- parseInstanceDeclaration
  members <- P.option [] $ do
    indented *> reserved "where"
    mark (P.many (same *> positioned parseValueDeclaration))
  return $ instanceDecl (ExplicitInstance members)

parseDerivingInstanceDeclaration :: TokenParser Declaration
parseDerivingInstanceDeclaration = do
  reserved "derive"
  ty <- P.option DerivedInstance (reserved "newtype" $> NewtypeInstance)
  instanceDecl <- parseInstanceDeclaration
  return $ instanceDecl ty

positioned :: TokenParser Declaration -> TokenParser Declaration
positioned = withSourceSpan PositionedDeclaration

-- | Parse a single declaration
parseDeclaration :: TokenParser Declaration
parseDeclaration = positioned (P.choice
                   [ parseDataDeclaration
                   , parseTypeDeclaration
                   , parseTypeSynonymDeclaration
                   , parseValueDeclaration
                   , parseExternDeclaration
                   , parseFixityDeclaration
                   , parseTypeClassDeclaration
                   , parseTypeInstanceDeclaration
                   , parseDerivingInstanceDeclaration
                   ]) P.<?> "declaration"

parseLocalDeclaration :: TokenParser Declaration
parseLocalDeclaration = positioned (P.choice
                   [ parseTypeDeclaration
                   , parseValueDeclaration
                   ] P.<?> "local declaration")

-- | Parse a module header and a collection of declarations
parseModule :: TokenParser Module
parseModule = do
  comments <- readComments
  start <- P.getPosition
  reserved "module"
  indented
  name <- moduleName
  exports <- P.optionMaybe $ parens $ commaSep1 parseDeclarationRef
  reserved "where"
  decls <- mark $ do
    -- TODO: extract a module header structure here, and provide a
    -- parseModuleHeader function. This should allow us to speed up rebuilds
    -- by only parsing as far as the module header. See PR #2054.
    imports <- P.many (same *> parseImportDeclaration)
    decls   <- P.many (same *> parseDeclaration)
    return (imports ++ decls)
  _ <- P.eof
  end <- P.getPosition
  let ss = SourceSpan (P.sourceName start) (toSourcePos start) (toSourcePos end)
  return $ Module ss comments name decls exports

-- | Parse a collection of modules in parallel
parseModulesFromFiles
  :: forall m k
   . MonadError MultipleErrors m
  => (k -> FilePath)
  -> [(k, Text)]
  -> m [(k, Module)]
parseModulesFromFiles toFilePath input =
  flip parU wrapError . inParallel . flip map input $ parseModuleFromFile toFilePath
  where
  wrapError :: Either P.ParseError a -> m a
  wrapError = either (throwError . MultipleErrors . pure . toPositionedError) return
  -- It is enough to force each parse result to WHNF, since success or failure can't be
  -- determined until the end of the file, so this effectively distributes parsing of each file
  -- to a different spark.
  inParallel :: [Either P.ParseError (k, a)] -> [Either P.ParseError (k, a)]
  inParallel = withStrategy (parList rseq)

-- | Parses a single module with FilePath for eventual parsing errors
parseModuleFromFile
  :: (k -> FilePath)
  -> (k, Text)
  -> Either P.ParseError (k, Module)
parseModuleFromFile toFilePath (k, content) = do
    let filename = toFilePath k
    ts <- lex filename content
    m <- runTokenParser filename parseModule ts
    pure (k, m)

-- | Converts a 'ParseError' into a 'PositionedError'
toPositionedError :: P.ParseError -> ErrorMessage
toPositionedError perr = ErrorMessage [ PositionedError (SourceSpan name start end) ] (ErrorParsingModule perr)
  where
  name   = (P.sourceName  . P.errorPos) perr
  start  = (toSourcePos . P.errorPos) perr
  end    = start

booleanLiteral :: TokenParser Bool
booleanLiteral = (reserved "true" >> return True) P.<|> (reserved "false" >> return False)

parseNumericLiteral :: TokenParser (Literal a)
parseNumericLiteral = NumericLiteral <$> number

parseCharLiteral :: TokenParser (Literal a)
parseCharLiteral = CharLiteral <$> charLiteral

parseStringLiteral :: TokenParser (Literal a)
parseStringLiteral = StringLiteral <$> stringLiteral

parseBooleanLiteral :: TokenParser (Literal a)
parseBooleanLiteral = BooleanLiteral <$> booleanLiteral

parseArrayLiteral :: TokenParser a -> TokenParser (Literal a)
parseArrayLiteral p = ArrayLiteral <$> squares (commaSep p)

parseObjectLiteral :: TokenParser (PSString, a) -> TokenParser (Literal a)
parseObjectLiteral p = ObjectLiteral <$> braces (commaSep p)

parseIdentifierAndValue :: TokenParser (PSString, Expr)
parseIdentifierAndValue =
  do
    name <- indented *> lname
    b <- P.option (Var $ Qualified Nothing (Ident name)) rest
    return (mkString name, b)
  <|> (,) <$> (indented *> stringLiteral) <*> rest
  where
  rest = indented *> colon *> indented *> parseValue

parseAbs :: TokenParser Expr
parseAbs = do
  symbol' "\\"
  args <- P.many1 (indented *> (Abs <$> (Left <$> parseIdent <|> Right <$> parseBinderNoParens)))
  indented *> rarrow
  value <- parseValue
  return $ toFunction args value
  where
  toFunction :: [Expr -> Expr] -> Expr -> Expr
  toFunction args value = foldr ($) value args

parseVar :: TokenParser Expr
parseVar = Var <$> parseQualified parseIdent

parseConstructor :: TokenParser Expr
parseConstructor = Constructor <$> parseQualified dataConstructorName

parseCase :: TokenParser Expr
parseCase = Case <$> P.between (reserved "case") (indented *> reserved "of") (commaSep1 parseValue)
                 <*> (indented *> mark (P.many1 (same *> mark parseCaseAlternative)))

parseCaseAlternative :: TokenParser CaseAlternative
parseCaseAlternative = CaseAlternative <$> commaSep1 parseBinder
                                       <*> (Left <$> (indented *>
                                                        P.many1 ((,) <$> parseGuard
                                                                     <*> (indented *> rarrow *> parseValue)
                                                                ))
                                            <|> Right <$> (indented *> rarrow *> parseValue))
                                       P.<?> "case alternative"

parseIfThenElse :: TokenParser Expr
parseIfThenElse = IfThenElse <$> (P.try (reserved "if") *> indented *> parseValue)
                             <*> (indented *> reserved "then" *> indented *> parseValue)
                             <*> (indented *> reserved "else" *> indented *> parseValue)

parseLet :: TokenParser Expr
parseLet = do
  reserved "let"
  indented
  ds <- mark $ P.many1 (same *> parseLocalDeclaration)
  indented
  reserved "in"
  result <- parseValue
  return $ Let ds result

parseValueAtom :: TokenParser Expr
parseValueAtom = withSourceSpan PositionedValue $ P.choice
                 [ parseAnonymousArgument
                 , Literal <$> parseNumericLiteral
                 , Literal <$> parseCharLiteral
                 , Literal <$> parseStringLiteral
                 , Literal <$> parseBooleanLiteral
                 , Literal <$> parseArrayLiteral parseValue
                 , Literal <$> parseObjectLiteral parseIdentifierAndValue
                 , parseAbs
                 , P.try parseConstructor
                 , P.try parseVar
                 , parseCase
                 , parseIfThenElse
                 , parseDo
                 , parseLet
                 , P.try $ Parens <$> parens parseValue
                 , Op <$> parseQualified (parens parseOperator)
                 , parseHole
                 ]

-- | Parse an expression in backticks or an operator
parseInfixExpr :: TokenParser Expr
parseInfixExpr
  = P.between tick tick parseValue
  <|> withSourceSpan PositionedValue (Op <$> parseQualified parseOperator)

parseHole :: TokenParser Expr
parseHole = Hole <$> holeLit

parsePropertyUpdate :: TokenParser (PSString, PathNode Expr)
parsePropertyUpdate = do
  name <- parseLabel
  updates <- parseShallowUpdate <|> parseNestedUpdate
  return (name, updates)
  where
    parseShallowUpdate :: TokenParser (PathNode Expr)
    parseShallowUpdate = Leaf <$> (indented *> equals *> indented *> parseValue)

    parseNestedUpdate :: TokenParser (PathNode Expr)
    parseNestedUpdate = Branch <$> parseUpdaterBodyFields

parseAccessor :: Expr -> TokenParser Expr
parseAccessor (Constructor _) = P.unexpected "constructor"
parseAccessor obj = P.try $ Accessor <$> (indented *> dot *> indented *> parseLabel) <*> pure obj

parseDo :: TokenParser Expr
parseDo = do
  reserved "do"
  indented
  Do <$> mark (P.many1 (same *> mark parseDoNotationElement))

parseDoNotationLet :: TokenParser DoNotationElement
parseDoNotationLet = DoNotationLet <$> (reserved "let" *> indented *> mark (P.many1 (same *> parseLocalDeclaration)))

parseDoNotationBind :: TokenParser DoNotationElement
parseDoNotationBind = DoNotationBind <$> P.try (parseBinder <* indented <* larrow) <*> parseValue

parseDoNotationElement :: TokenParser DoNotationElement
parseDoNotationElement = P.choice
            [ parseDoNotationBind
            , parseDoNotationLet
            , DoNotationValue <$> parseValue
            ]

-- | Expressions including indexers and record updates
indexersAndAccessors :: TokenParser Expr
indexersAndAccessors = buildPostfixParser postfixTable parseValueAtom
  where
  postfixTable = [ parseAccessor
                 , P.try . parseUpdaterBody
                 ]

-- | Parse an expression
parseValue :: TokenParser Expr
parseValue = withSourceSpan PositionedValue
  (P.buildExpressionParser operators
    . buildPostfixParser postfixTable
    $ indexersAndAccessors) P.<?> "expression"
  where
  postfixTable = [ \v -> P.try (flip App <$> (indented *> indexersAndAccessors)) <*> pure v
                 , \v -> flip (TypedValue True) <$> (indented *> doubleColon *> parsePolyType) <*> pure v
                 ]
  operators = [ [ P.Prefix (indented *> symbol' "-" *> return UnaryMinus)
                ]
              , [ P.Infix (P.try (indented *> parseInfixExpr P.<?> "infix expression") >>= \ident ->
                    return (BinaryNoParens ident)) P.AssocRight
                ]
              ]

parseUpdaterBodyFields :: TokenParser (PathTree Expr)
parseUpdaterBodyFields = do
  updates <- indented *> braces (commaSep1 (indented *> parsePropertyUpdate))
  (_, tree) <- foldM insertUpdate (S.empty, []) updates
  return (PathTree (AssocList (reverse tree)))
  where
    insertUpdate (seen, xs) (key, node)
      | S.member key seen = P.unexpected ("Duplicate key in record update: " ++ show key)
      | otherwise = return (S.insert key seen, (key, node) : xs)

parseUpdaterBody :: Expr -> TokenParser Expr
parseUpdaterBody v = ObjectUpdateNested v <$> parseUpdaterBodyFields

parseAnonymousArgument :: TokenParser Expr
parseAnonymousArgument = underscore *> pure AnonymousArgument

parseNumberLiteral :: TokenParser Binder
parseNumberLiteral = LiteralBinder . NumericLiteral <$> (sign <*> number)
  where
  sign :: TokenParser (Either Integer Double -> Either Integer Double)
  sign = (symbol' "-" >> return (negate +++ negate))
         <|> (symbol' "+" >> return id)
         <|> return id

parseNullaryConstructorBinder :: TokenParser Binder
parseNullaryConstructorBinder = ConstructorBinder <$> parseQualified dataConstructorName <*> pure []

parseConstructorBinder :: TokenParser Binder
parseConstructorBinder = ConstructorBinder <$> parseQualified dataConstructorName <*> many (indented *> parseBinderNoParens)

parseObjectBinder:: TokenParser Binder
parseObjectBinder = LiteralBinder <$> parseObjectLiteral (indented *> parseIdentifierAndBinder)

parseArrayBinder :: TokenParser Binder
parseArrayBinder = LiteralBinder <$> parseArrayLiteral (indented *> parseBinder)

parseVarOrNamedBinder :: TokenParser Binder
parseVarOrNamedBinder = do
  name <- parseIdent
  let parseNamedBinder = NamedBinder name <$> (at *> indented *> parseBinderAtom)
  parseNamedBinder <|> return (VarBinder name)

parseNullBinder :: TokenParser Binder
parseNullBinder = underscore *> return NullBinder

parseIdentifierAndBinder :: TokenParser (PSString, Binder)
parseIdentifierAndBinder =
    do name <- lname
       b <- P.option (VarBinder (Ident name)) rest
       return (mkString name, b)
    <|> (,) <$> stringLiteral <*> rest
  where
    rest = indented *> colon *> indented *> parseBinder

-- | Parse a binder
parseBinder :: TokenParser Binder
parseBinder =
  withSourceSpan
    PositionedBinder
    ( P.buildExpressionParser operators
    . buildPostfixParser postfixTable
    $ parseBinderAtom
    )
  where
  operators =
    [ [ P.Infix (P.try (indented *> parseOpBinder P.<?> "binder operator") >>= \op ->
          return (BinaryNoParensBinder op)) P.AssocRight
      ]
    ]

  -- TODO: parsePolyType when adding support for polymorphic types
  postfixTable = [ \b -> flip TypedBinder b <$> (indented *> doubleColon *> parseType)
                 ]

  parseOpBinder :: TokenParser Binder
  parseOpBinder = OpBinder <$> parseQualified parseOperator

parseBinderAtom :: TokenParser Binder
parseBinderAtom = P.choice
  [ parseNullBinder
  , LiteralBinder <$> parseCharLiteral
  , LiteralBinder <$> parseStringLiteral
  , LiteralBinder <$> parseBooleanLiteral
  , parseNumberLiteral
  , parseVarOrNamedBinder
  , parseConstructorBinder
  , parseObjectBinder
  , parseArrayBinder
  , ParensInBinder <$> parens parseBinder
  ] P.<?> "binder"

-- | Parse a binder as it would appear in a top level declaration
parseBinderNoParens :: TokenParser Binder
parseBinderNoParens = P.choice
                      [ parseNullBinder
                      , LiteralBinder <$> parseCharLiteral
                      , LiteralBinder <$> parseStringLiteral
                      , LiteralBinder <$> parseBooleanLiteral
                      , parseNumberLiteral
                      , parseVarOrNamedBinder
                      , parseNullaryConstructorBinder
                      , parseObjectBinder
                      , parseArrayBinder
                      , ParensInBinder <$> parens parseBinder
                      ] P.<?> "binder"

-- | Parse a guard
parseGuard :: TokenParser Guard
parseGuard = pipe *> indented *> parseValue
