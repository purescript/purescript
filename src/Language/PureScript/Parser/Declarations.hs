-- | Parsers for module definitions and declarations
module Language.PureScript.Parser.Declarations
  ( parseDeclaration
  , parseDeclarationRef
  , parseModule
  , parseModuleDeclaration
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
import           Control.Monad (foldM, join)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Parallel.Strategies (withStrategy, parList, rseq)
import           Control.Monad.Identity
import           Data.Functor (($>))
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
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
import           Language.PureScript.Parser.State
import           Language.PureScript.PSString (PSString, mkString)
import           Language.PureScript.Types
import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as P

kindedIdent :: TokenParser (Text, Maybe Kind)
kindedIdent = (, Nothing) <$> identifier
          <|> parens ((,) <$> identifier <*> (Just <$> (indented *> doubleColon *> indented *> parseKind)))

parseDataDeclaration :: TokenParser Declaration
parseDataDeclaration = withSourceAnnF $ do
  dtype <- (reserved "data" *> return Data) <|> (reserved "newtype" *> return Newtype)
  name <- indented *> typeName
  tyArgs <- many (indented *> kindedIdent)
  ctors <- P.option [] $ do
    indented *> equals
    P.sepBy1 ((,) <$> dataConstructorName <*> P.many (indented *> noWildcards parseTypeAtom)) pipe
  return $ \sa -> DataDeclaration sa dtype name tyArgs ctors

parseTypeDeclaration :: TokenParser Declaration
parseTypeDeclaration = withSourceAnnF $ do
  name <- P.try (parseIdent <* indented <* doubleColon)
  ty <- parsePolyType
  return $ \sa -> TypeDeclaration sa name ty

parseTypeSynonymDeclaration :: TokenParser Declaration
parseTypeSynonymDeclaration = withSourceAnnF $ do
  name <- reserved "type" *> indented *> typeName
  vars <- many (indented *> kindedIdent)
  ty <- indented *> equals *> noWildcards parsePolyType
  return $ \sa -> TypeSynonymDeclaration sa name vars ty

parseValueWithWhereClause :: TokenParser Expr
parseValueWithWhereClause = withSourceAnnF $ do
  indented
  value <- parseValue
  whereClause <- P.optionMaybe $ do
    indented
    reserved "where"
    indented
    mark $ P.many1 (same *> parseLocalDeclaration)
  return $ \sa -> maybe value (flip (Let sa) value) whereClause

parseValueWithIdentAndBinders :: Ident -> [Binder] -> TokenParser (SourceAnn -> Declaration)
parseValueWithIdentAndBinders ident bs = do
  value <- indented *> (unguarded <|> guarded)
  return $ \sa -> ValueDeclaration sa ident Public bs value
  where
    unguarded = withSourceSpanF $ do
      v <- equals *> parseValueWithWhereClause
      return $ \sa -> [MkUnguarded sa v]
    guarded = P.many1 . withSourceSpanF $ do
      g <- parseGuard
      v <- indented *> equals *> parseValueWithWhereClause
      return $ \sa -> GuardedExpr sa g v

parseValueDeclaration :: TokenParser Declaration
parseValueDeclaration = withSourceAnnF $ do
  ident <- parseIdent
  binders <- P.many parseBinderNoParens
  parseValueWithIdentAndBinders ident binders

parseLocalValueDeclaration :: TokenParser Declaration
parseLocalValueDeclaration = withSourceAnnF .
    join $ go <$> parseBinder <*> P.many parseBinderNoParens
  where
  go :: Binder -> [Binder] -> TokenParser (SourceAnn -> Declaration)
  go (VarBinder _ ident) bs = parseValueWithIdentAndBinders ident bs
  go binder [] = do
    boot <- indented *> equals *> parseValueWithWhereClause
    return $ \sa -> BoundValueDeclaration sa binder boot
  go _ _ = P.unexpected "patterns in local value declaration"

parseExternDeclaration :: TokenParser Declaration
parseExternDeclaration = withSourceAnnF $
  reserved "foreign" *>
  indented *> reserved "import" *>
  indented *> (parseExternData <|> P.try parseExternKind <|> parseExternTerm)
  where
  parseExternData =
    (\name kind sa -> ExternDataDeclaration sa name kind)
      <$> (reserved "data" *> indented *> typeName)
      <*> (indented *> doubleColon *> parseKind)
  parseExternKind =
    flip ExternKindDeclaration
      <$> (reserved "kind" *> indented *> kindName)
  parseExternTerm =
    (\name ty sa -> ExternDeclaration sa name ty)
      <$> parseIdent
      <*> (indented *> doubleColon *> noWildcards parsePolyType)

parseAssociativity :: TokenParser Associativity
parseAssociativity =
  (reserved "infixl" *> return Infixl) <|>
  (reserved "infixr" *> return Infixr) <|>
  (reserved "infix"  *> return Infix)

parseFixity :: TokenParser Fixity
parseFixity = Fixity <$> parseAssociativity <*> (indented *> natural)

parseFixityDeclaration :: TokenParser Declaration
parseFixityDeclaration = withSourceAnnF $ do
  fixity <- parseFixity
  indented
  def <- (Right <$> typeFixity fixity) <|> (Left <$> valueFixity fixity)
  return $ \sa -> FixityDeclaration sa def
  where
  typeFixity fixity =
    TypeFixity fixity
      <$> (reserved "type" *> parseQualified typeName)
      <*> (reserved "as" *> parseOperator)
  valueFixity fixity =
    ValueFixity fixity
      <$> parseQualified ((Left <$> parseIdent) <|> (Right <$> dataConstructorName))
      <*> (reserved "as" *> parseOperator)

parseImportDeclaration :: TokenParser Declaration
parseImportDeclaration = withSourceAnnF $ do
  (mn, declType, asQ) <- parseImportDeclaration'
  return $ \sa -> ImportDeclaration sa mn declType asQ

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
    withSourceSpan' KindRef (P.try (reserved "kind" *> kindName))
    <|> withSourceSpan' ValueRef parseIdent
    <|> withSourceSpan' ValueOpRef (parens parseOperator)
    <|> withSourceSpan' (\sa -> ($ TypeRef sa)) parseTypeRef
    <|> withSourceSpan' TypeClassRef (reserved "class" *> properName)
    <|> withSourceSpan' ModuleRef (indented *> reserved "module" *> moduleName)
    <|> withSourceSpan' TypeOpRef (indented *> reserved "type" *> parens parseOperator)
  where
  parseTypeRef = do
    name <- typeName
    dctors <- P.optionMaybe $ parens (symbol' ".." *> pure Nothing <|> Just <$> commaSep dataConstructorName)
    return $ \f -> f name (fromMaybe (Just []) dctors)

parseTypeClassDeclaration :: TokenParser Declaration
parseTypeClassDeclaration = withSourceAnnF $ do
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
    indented *> mark (P.many (same *> parseTypeDeclaration))
  return $ \sa -> TypeClassDeclaration sa className idents implies dependencies members

parseConstraint :: TokenParser Constraint
parseConstraint = Constraint <$> parseQualified properName
                             <*> P.many (noWildcards $ noForAll parseTypeAtom)
                             <*> pure Nothing

parseInstanceDeclaration :: TokenParser (TypeInstanceBody -> Declaration)
parseInstanceDeclaration = withSourceAnnF $ do
  reserved "instance"
  name <- parseIdent <* indented <* doubleColon
  deps <- P.optionMaybe . P.try $ do
    deps <- (return <$> parseConstraint) <|> parens (commaSep1 parseConstraint)
    indented
    rfatArrow
    return deps
  className <- indented *> parseQualified properName
  ty <- P.many (indented *> parseTypeAtom)
  return $ \sa -> TypeInstanceDeclaration sa name (fromMaybe [] deps) className ty

parseTypeInstanceDeclaration :: TokenParser Declaration
parseTypeInstanceDeclaration = do
  instanceDecl <- parseInstanceDeclaration
  members <- P.option [] $ do
    indented *> reserved "where"
    mark (P.many (same *> declsInInstance))
  return $ instanceDecl (ExplicitInstance members)
  where
    declsInInstance :: TokenParser Declaration
    declsInInstance = P.choice
      [ parseTypeDeclaration
      , parseValueDeclaration
      ] P.<?> "type declaration or value declaration in instance"

parseDerivingInstanceDeclaration :: TokenParser Declaration
parseDerivingInstanceDeclaration = do
  reserved "derive"
  ty <- P.option DerivedInstance (reserved "newtype" $> NewtypeInstance)
  instanceDecl <- parseInstanceDeclaration
  return $ instanceDecl ty

-- | Parse a single declaration
parseDeclaration :: TokenParser Declaration
parseDeclaration =
  P.choice
    [ parseDataDeclaration
    , parseTypeDeclaration
    , parseTypeSynonymDeclaration
    , parseValueDeclaration
    , parseExternDeclaration
    , parseFixityDeclaration
    , parseTypeClassDeclaration
    , parseTypeInstanceDeclaration
    , parseDerivingInstanceDeclaration
    ] P.<?> "declaration"

parseLocalDeclaration :: TokenParser Declaration
parseLocalDeclaration =
  P.choice
    [ parseTypeDeclaration
    , parseLocalValueDeclaration
    ] P.<?> "local declaration"

-- | Parse a module declaration and its export declarations
parseModuleDeclaration :: TokenParser (ModuleName, Maybe [DeclarationRef])
parseModuleDeclaration = do
  reserved "module"
  indented
  name <- moduleName
  exports <- P.optionMaybe . parens $ commaSep1 parseDeclarationRef
  reserved "where"
  pure (name, exports)

-- | Parse a module header and a collection of declarations
parseModule :: TokenParser Module
parseModule = do
  comments <- readComments
  start <- P.getPosition
  (name, exports) <- parseModuleDeclaration
  decls <- mark $ do
    -- TODO: extract a module header structure here, and provide a
    -- parseModuleHeader function. This should allow us to speed up rebuilds
    -- by only parsing as far as the module header. See PR #2054.
    imports <- P.many (same *> parseImportDeclaration)
    decls   <- P.many (same *> parseDeclaration)
    return (imports <> decls)
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
  flip parU wrapError . inParallel . flip fmap input $ parseModuleFromFile toFilePath
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
    (sa, name) <- indented *> withSourceAnnF (flip (,) <$> lname)
    b <- P.option (Var sa $ Qualified Nothing (Ident name)) rest
    return (mkString name, b)
  <|> (,) <$> (indented *> stringLiteral) <*> rest
  where
  rest = indented *> colon *> indented *> parseValue

parseAbs :: TokenParser Expr
parseAbs = do
  symbol' "\\"
  args <- P.many1 (indented *> withSourceAnnF (flip Abs <$> parseBinderNoParens))
  indented *> rarrow
  value <- parseValue
  return $ toFunction args value
  where
  toFunction :: [Expr -> Expr] -> Expr -> Expr
  toFunction args value = foldr ($) value args

parseVar :: TokenParser Expr
parseVar = withSourceAnnF $ flip Var <$> parseQualified parseIdent

parseConstructor :: TokenParser Expr
parseConstructor = withSourceAnnF $ flip Constructor <$> parseQualified dataConstructorName

parseCase :: TokenParser Expr
parseCase = withSourceAnnF $ do
  e <- P.between (reserved "case") (indented *> reserved "of") (commaSep1 parseValue)
  cas <- indented *> mark (P.many1 (same *> mark parseCaseAlternative))
  return $ \sa -> Case sa e cas

parseCaseAlternative :: TokenParser CaseAlternative
parseCaseAlternative = withSourceSpanF (do
  bs <- commaSep1 parseBinder
  g <- indented *> (unguarded <|> guarded)
  return $ \sa -> CaseAlternative sa bs g) P.<?> "case alternative"
  where
    unguarded = withSourceSpanF $ do
      v <- rarrow *> parseValue
      return $ \sa -> [MkUnguarded sa v]
    guarded = P.many1 . withSourceSpanF $ do
      g <- parseGuard
      v <- indented *> rarrow *> parseValue
      return $ \sa -> GuardedExpr sa g v

parseIfThenElse :: TokenParser Expr
parseIfThenElse = withSourceAnnF $ do
  c <- P.try (reserved "if") *> indented *> parseValue
  t <- indented *> reserved "then" *> indented *> parseValue
  f <- indented *> reserved "else" *> indented *> parseValue
  return $ \sa -> IfThenElse sa c t f

parseLet :: TokenParser Expr
parseLet = withSourceAnnF $ do
  reserved "let"
  indented
  ds <- mark $ P.many1 (same *> parseLocalDeclaration)
  indented
  reserved "in"
  result <- parseValue
  return $ \sa -> Let sa ds result

parseValueAtom :: TokenParser Expr
parseValueAtom = P.choice
  [ parseAnonymousArgument
  , withSourceAnnF $ flip Literal <$> parseNumericLiteral
  , withSourceAnnF $ flip Literal <$> parseCharLiteral
  , withSourceAnnF $ flip Literal <$> parseStringLiteral
  , withSourceAnnF $ flip Literal <$> parseBooleanLiteral
  , withSourceAnnF $ flip Literal <$> parseArrayLiteral parseValue
  , withSourceAnnF $ flip Literal <$> parseObjectLiteral parseIdentifierAndValue
  , parseAbs
  , P.try parseConstructor
  , P.try parseVar
  , parseCase
  , parseIfThenElse
  , parseDo
  , parseLet
  , P.try . withSourceAnnF $ flip Parens <$> parens parseValue
  , withSourceAnnF $ flip Op <$> parseQualified (parens parseOperator)
  , parseHole
  ]

parseHole :: TokenParser Expr
parseHole = withSourceAnnF $ flip Hole <$> holeLit

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

parseDo :: TokenParser Expr
parseDo = withSourceAnnF $ do
  reserved "do"
  indented
  flip Do <$> mark (P.many1 (same *> mark parseDoNotationElement))

parseDoNotationLet :: TokenParser DoNotationElement
parseDoNotationLet = withSourceAnnF $
  flip DoNotationLet
    <$> (reserved "let" *> indented *> mark (P.many1 (same *> parseLocalDeclaration)))

parseDoNotationBind :: TokenParser DoNotationElement
parseDoNotationBind = withSourceAnnF $ do
  b <- P.try (parseBinder <* indented <* larrow)
  v <- parseValue
  return $ \sa -> DoNotationBind sa b v

parseDoNotationElement :: TokenParser DoNotationElement
parseDoNotationElement = P.choice
  [ parseDoNotationBind
  , parseDoNotationLet
  , withSourceAnnF $ flip DoNotationValue <$> parseValue
  ]

-- | Expressions including indexers and record updates
indexersAndAccessors :: TokenParser Expr
indexersAndAccessors = buildPostfixParser postfixTable parseValueAtom
  where
  postfixTable =
    [ parseAccessor
    , P.try . parseUpdaterBody
    ]

  parseAccessor :: Expr -> TokenParser Expr
  parseAccessor (Constructor _ _) = P.unexpected "constructor"
  parseAccessor obj = P.try . withSourceAnnF $ do
    label <- indented *> dot *> indented *> parseLabel
    return $ \sa -> Accessor sa label obj

-- | Parse an expression
parseValue :: TokenParser Expr
parseValue =
  P.buildExpressionParser operators
    (buildPostfixParser postfixTable indexersAndAccessors)
    P.<?> "expression"
  where
    operators :: P.OperatorTable [PositionedToken] ParseState Identity Expr
    operators =
      [ [P.Prefix (indented *> withSourceAnnF (symbol' "-" *> return UnaryMinus))]
      , [P.Infix infixOp P.AssocRight]
      ]

    infixOp :: TokenParser (Expr -> Expr -> Expr)
    infixOp = do
      ident <- P.try (indented *> parseInfixExpr P.<?> "infix expression")
      return $ \lhs rhs ->
        let ss = mergeSourceSpan (exprSourceSpan lhs) (exprSourceSpan rhs)
        in BinaryNoParens (ss, []) ident lhs rhs

    postfixTable :: [Expr -> TokenParser Expr]
    postfixTable = [P.try . postfixOpApp, postfixOpType]

    postfixOpApp :: Expr -> TokenParser Expr
    postfixOpApp v = withSourceAnnF $ do
      ixa <- indented *> indexersAndAccessors
      -- TODO: check v / ixa order
      return $ \sa -> App sa v ixa

    postfixOpType :: Expr -> TokenParser Expr
    postfixOpType v = withSourceAnnF $ do
      ty <- indented *> doubleColon *> parsePolyType
      return $ \sa -> TypedValue sa True v ty

    parseInfixExpr :: TokenParser Expr
    parseInfixExpr
      = P.between tick tick parseValue
      <|> withSourceAnnF (flip Op <$> parseQualified parseOperator)

parseUpdaterBodyFields :: TokenParser (PathTree Expr)
parseUpdaterBodyFields = do
  updates <- indented *> braces (commaSep1 (indented *> parsePropertyUpdate))
  (_, tree) <- foldM insertUpdate (S.empty, []) updates
  return (PathTree (AssocList (reverse tree)))
  where
    insertUpdate (seen, xs) (key, node)
      | S.member key seen = P.unexpected ("Duplicate key in record update: " <> show key)
      | otherwise = return (S.insert key seen, (key, node) : xs)

parseUpdaterBody :: Expr -> TokenParser Expr
parseUpdaterBody v = withSourceAnnF $
  (\fs sa -> ObjectUpdateNested sa v fs) <$> parseUpdaterBodyFields

parseAnonymousArgument :: TokenParser Expr
parseAnonymousArgument = withSourceAnnF $
  underscore *> pure AnonymousArgument

parseNumberLiteral :: TokenParser Binder
parseNumberLiteral = withSourceSpanF $
  (\n ss -> LiteralBinder ss (NumericLiteral n)) <$> (sign <*> number)
  where
  sign :: TokenParser (Either Integer Double -> Either Integer Double)
  sign = (symbol' "-" >> return (negate +++ negate))
         <|> (symbol' "+" >> return id)
         <|> return id

parseNullaryConstructorBinder :: TokenParser Binder
parseNullaryConstructorBinder = withSourceSpanF $
  (\name ss -> ConstructorBinder ss name [])
    <$> parseQualified dataConstructorName

parseConstructorBinder :: TokenParser Binder
parseConstructorBinder = withSourceSpanF $
  (\name args ss -> ConstructorBinder ss name args)
    <$> parseQualified dataConstructorName
    <*> many (indented *> parseBinderNoParens)

parseObjectBinder:: TokenParser Binder
parseObjectBinder = withSourceSpanF $
  flip LiteralBinder <$> parseObjectLiteral (indented *> parseEntry)
  where
    parseEntry :: TokenParser (PSString, Binder)
    parseEntry = var <|> (,) <$> stringLiteral <*> rest
      where
        var = withSourceSpanF $ do
          name <- lname
          b <- P.option (\ss -> VarBinder ss (Ident name)) (const <$> rest)
          return $ \ss -> (mkString name, b ss)
        rest = indented *> colon *> indented *> parseBinder

parseArrayBinder :: TokenParser Binder
parseArrayBinder = withSourceSpanF $
  flip LiteralBinder <$> parseArrayLiteral (indented *> parseBinder)

parseVarOrNamedBinder :: TokenParser Binder
parseVarOrNamedBinder = withSourceSpanF $ do
  name <- parseIdent
  let parseNamedBinder = (\b ss -> NamedBinder ss name b) <$> (at *> indented *> parseBinderAtom)
  parseNamedBinder <|> return (`VarBinder` name)

parseNullBinder :: TokenParser Binder
parseNullBinder = withSourceSpanF $ underscore *> return NullBinder

-- | Parse a binder
parseBinder :: TokenParser Binder
parseBinder =
  P.buildExpressionParser operators
    (buildPostfixParser postfixTable parseBinderAtom)
  where
    operators :: P.OperatorTable [PositionedToken] ParseState Identity Binder
    operators = [[P.Infix infixOp P.AssocRight]]

    infixOp :: TokenParser (Binder -> Binder -> Binder)
    infixOp = do
      op <- P.try (indented *> parseOpBinder P.<?> "binder operator")
      return $ \lhs rhs ->
        let ss = mergeSourceSpan (binderSourceSpan lhs) (binderSourceSpan rhs)
        in BinaryNoParensBinder ss op lhs rhs

    postfixTable :: [Binder -> TokenParser Binder]
    postfixTable = [postfixOp]

    postfixOp :: Binder -> TokenParser Binder
    postfixOp b = withSourceSpanF $ do
      ty <- indented *> doubleColon *> parsePolyType
      return $ \ss -> TypedBinder ss ty b

    parseOpBinder :: TokenParser Binder
    parseOpBinder = withSourceSpanF $ flip OpBinder <$> parseQualified parseOperator

parseBinderAtom :: TokenParser Binder
parseBinderAtom =
  P.choice
   [ parseNullBinder
   , withSourceSpanF $ flip LiteralBinder <$> parseCharLiteral
   , withSourceSpanF $ flip LiteralBinder <$> parseStringLiteral
   , withSourceSpanF $ flip LiteralBinder <$> parseBooleanLiteral
   , parseNumberLiteral
   , parseVarOrNamedBinder
   , parseConstructorBinder
   , parseObjectBinder
   , parseArrayBinder
   , withSourceSpanF $ flip ParensInBinder <$> parens parseBinder
   ] P.<?> "binder"

-- | Parse a binder as it would appear in a top level declaration
parseBinderNoParens :: TokenParser Binder
parseBinderNoParens =
  P.choice
    [ parseNullBinder
    , withSourceSpanF $ flip LiteralBinder <$> parseCharLiteral
    , withSourceSpanF $ flip LiteralBinder <$> parseStringLiteral
    , withSourceSpanF $ flip LiteralBinder <$> parseBooleanLiteral
    , parseNumberLiteral
    , parseVarOrNamedBinder
    , parseNullaryConstructorBinder
    , parseObjectBinder
    , parseArrayBinder
    , withSourceSpanF $ flip ParensInBinder <$> parens parseBinder
    ] P.<?> "binder"

-- | Parse a guard
parseGuard :: TokenParser [Guard]
parseGuard =
  pipe *> indented *> P.sepBy1 (parsePatternGuard <|> parseConditionGuard) comma
  where
    parsePatternGuard = withSourceSpanF $ do
      b <- P.try (parseBinder <* indented <* larrow)
      v <- parseValue
      return $ \sa -> PatternGuard sa b v
    parseConditionGuard =
      withSourceSpanF $ flip ConditionGuard <$> parseValue
