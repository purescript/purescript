-- |
-- Read the core functional representation from JSON format
--

module Language.PureScript.CoreFn.FromJSON
  ( moduleFromJSON
  ) where

import Prelude.Compat

import Control.Applicative ((<|>))
import Control.Arrow ((***))
import Control.Monad ((>=>))
import Data.Aeson
import Data.Aeson.Types (Parser, Value, modifyFailure)
import Data.List (init, last)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Text.ParserCombinators.ReadP (readP_to_S)
import Data.Traversable (for)
import qualified Data.Vector as V
import Data.Vector ((!?))
import Data.Version (Version, parseVersion)

import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.AST.Literals
import Language.PureScript.CoreFn.Ann
import Language.PureScript.CoreFn
import Language.PureScript.Names
import Language.PureScript.PSString (PSString, mkString)

parseArray :: (Value -> Parser a) -> Array -> Parser [a]
parseArray p = sequence . foldr (\a as -> p a : as) []

constructorTypeFromJSON :: Value -> Parser ConstructorType
constructorTypeFromJSON v = do
  t <- parseJSON v
  case t of
    "ProductType" -> return ProductType
    "SumType"     -> return SumType
    _             -> fail ("not regonized ConstructorType: " ++ T.unpack t)

metaFromJSON :: Value -> Parser (Maybe Meta)
metaFromJSON Null = return Nothing
metaFromJSON v = Just <$> (isConstructorFromJSON <|> isOtherFromJSON)
  where
    isConstructorFromJSON = do
      ("IsConstructor", ctJ, is) <- parseJSON v :: Parser (Text, Value, [Text])
      ct <- constructorTypeFromJSON ctJ
      return $ IsConstructor ct (Ident <$> is)

    isOtherFromJSON = do
      t <- parseJSON v
      case t of
        "IsNewtype"               -> return IsNewtype
        "IsTypeClassConstructor"  -> return IsTypeClassConstructor
        "IsForeign"               -> return IsForeign
        _                         -> fail ("not regonized Meta: " ++ T.unpack t)

annFromJSON :: Value -> Parser Ann
annFromJSON v = modifyFailure (("addFromJSON error: " ++ show v ++ " ") ++) $ do
  ("Ann", ss, metaJ) <- parseJSON v :: Parser (String, SourceSpan, Value)
  meta <- metaFromJSON metaJ
  return (ss, [], Nothing, meta)

literalFromJSON :: (Value -> Parser a) -> Value -> Parser (Literal a)
literalFromJSON t v =
        parseIntLiteral
    <|> parseNumberLiteral
    <|> parseStringLiteral
    <|> parseCharLiteral
    <|> parseBooleanLiteral
    <|> parseArrayLiteral
    <|> parseObjectLiteral
    <|> fail ("error parsing literal: " ++ show v)
  where

  parseIntLiteral = do
    ("IntLiteral", n) <- parseJSON v :: Parser (String, Integer)
    return $ NumericLiteral (Left n)

  parseNumberLiteral = do
    ("NumberLiteral", n) <- parseJSON v :: Parser (String, Double)
    return $ NumericLiteral (Right n)

  parseStringLiteral = do
    ("StringLiteral", s) <- parseJSON v :: Parser (String, PSString)
    return $ StringLiteral s

  parseCharLiteral = do
    ("CharLiteral", c) <- parseJSON v :: Parser (String, Char)
    return $ CharLiteral c

  parseBooleanLiteral = do
    ("BooleanLiteral", b) <- parseJSON v :: Parser (String, Bool)
    return $ BooleanLiteral b

  parseArrayLiteral = do
    ("ArrayLiteral", vals) <- parseJSON v :: Parser (String, Array)
    as <- mapM t (V.toList vals)
    return $ ArrayLiteral as

  parseObjectLiteral = do
    ("ObjectLiteral", o) <- parseJSON v :: Parser (String, Object)
    ObjectLiteral <$>
      for (HM.toList o) (sequence . (mkString *** t))

properNameFromJSON :: Value -> Parser (ProperName a)
properNameFromJSON = fmap ProperName . parseJSON

qualifiedFromJSON :: (Text -> a) -> Value -> Parser (Qualified a)
qualifiedFromJSON f = parseJSON >=> parseQualified
  where
  unsnoc :: [a] -> Maybe ([a], a)
  unsnoc [] = Nothing
  unsnoc as = Just (init as, last as)

  parseQualified t = case unsnoc (T.splitOn "." t) of
    Just (as, a)  | length as > 0 -> return $ Qualified (Just . ModuleName $ ProperName <$> as) (f a)
                  | otherwise     -> return $ Qualified Nothing (f a)
    Nothing                       -> fail "qualifiedFromJSON: absurd"

moduleFromJSON :: Value -> Parser (Version, ModuleT () Ann)
moduleFromJSON = withObject "Module" $
      \o -> case HM.foldrWithKey (curry ((:) . (moduleNameFromString *** id))) [] o of
              (mn, m):[] -> (,) <$> versionFromJSON m <*> moduleFromJSON' mn m
              _          -> fail "expected single module"
  where
  versionFromJSON :: Value -> Parser Version
  versionFromJSON v = do
    ver <- parseJSON v >>= (.: "builtWith")
    case readP_to_S parseVersion ver of
      (r, _) : _ -> return r
      _ -> fail "failed parsing purs version"

  parseIdent :: Value -> Parser Ident
  parseIdent = fmap Ident . parseJSON

  importFromJSON :: Value -> Parser (Ann, ModuleName)
  importFromJSON v = do
    (annJ, mn) <- parseJSON v :: Parser (Value, Text)
    ann <- annFromJSON annJ
    return (ann, moduleNameFromString mn)

  moduleFromJSON' :: ModuleName -> Value -> Parser (ModuleT () Ann)
  moduleFromJSON' moduleName v = do
    o <- parseJSON v
    moduleImports <- o .: "imports"
      >>= withArray "imports" (parseArray importFromJSON)
    me <- o .: "exports"
    let moduleExports = Ident <$> me
    moduleDecls <- o .: "decls"
      >>= withArray "declarations" (parseArray bindFromJSON)
    moduleForeign <- o .: "foreign"
      >>= withArray "foreign" (parseArray (fmap (flip (,) ()) . parseIdent))
    -- moduleComments are not in the CoreFn json representation
    let moduleComments = []
    return $ Module {..}

bindFromJSON :: Value -> Parser (Bind Ann)
bindFromJSON = fmap toBind <$> withArray "Binds" (parseArray parseBind)
  where
  parseBind :: Value -> Parser (Ident, Ann, Expr Ann)
  parseBind v = do
    (t, ann, e) <- parseJSON v :: Parser (Text, Value, Value)
    (Ident t,,) <$> annFromJSON ann <*> exprFromJSON e

  toBind :: [(Ident, Ann, Expr Ann)] -> Bind Ann
  toBind [] = Rec []
  toBind [(i, ann, e)] = NonRec ann i e
  toBind l = Rec (map (\(i, ann, e) -> ((ann, i), e)) l)

tag :: Value -> Parser (Maybe Text)
tag = withArray "tag" (sequence . fmap parseJSON . (!? 0))

exprFromJSON :: Value -> Parser (Expr Ann)
exprFromJSON v = do
  -- explicitly pull the tag so we get errors from the correct parser
  t <- tag v
  case t of
    Just "Var"          -> modifyFailure ("exprFromJSON (Var): " ++) varFromJSON
    Just "Literal"      -> modifyFailure ("exprFromJSON (Literal): " ++) literalExprFromJSON
    Just "Constructor"  -> modifyFailure ("exprFromJSON (Constructor): " ++) constructorFromJSON
    Just "Accessor"     -> modifyFailure ("exprFromJSON (Accessor): " ++) accessorFromJSON
    Just "ObjectUpdate" -> modifyFailure ("exprFromJSON (ObjectUpdate): " ++) objectUpdateFromJSON
    Just "Abs"          -> modifyFailure ("exprFromJSON (Abs): " ++) absFromJSON
    Just "App"          -> modifyFailure ("exprFromJSON (App): " ++) appFromJSON
    Just "Case"         -> modifyFailure ("exprFromJSON (Case): " ++) caseFromJSON
    Just "Let"          -> modifyFailure ("exprFromJSON (Let): " ++) letFromJSON
    Just s              -> fail ("exprFromJSON: not recognized expression: " ++ T.unpack s)
    Nothing             -> fail ("exprFromJSON: error parsing expression: " ++ show v)

  where
  varFromJSON = do
    (_, ann, i) <- parseJSON v :: Parser (Text, Value, Value)
    Var <$> annFromJSON ann <*> qualifiedFromJSON Ident i

  literalExprFromJSON = do
    (_, ann, l) <- parseJSON v :: Parser (Text, Value, Value)
    Literal <$> annFromJSON ann <*> literalFromJSON exprFromJSON l

  constructorFromJSON = do
    (_, ann, d, c, is) <- parseJSON v :: Parser (Text, Value, Value, Value, [Text])
    Constructor
      <$> annFromJSON ann
      <*> properNameFromJSON d
      <*> properNameFromJSON c
      <*> return (Ident <$> is)

  accessorFromJSON = do
    (_, ann, t, e) <- parseJSON v :: Parser (Text, Value, Value, Value)
    Accessor
      <$> annFromJSON ann
      <*> parseJSON t
      <*> exprFromJSON e

  objectUpdateFromJSON = do
    (_, ann, r, fs) <- parseJSON v :: Parser (Text, Value, Value, Object)
    ObjectUpdate
      <$> annFromJSON ann
      <*> exprFromJSON r
      <*> for (HM.toList fs) (sequence . (mkString *** exprFromJSON))

  absFromJSON = do
    (_, annJ, i, e) <- parseJSON v :: Parser (Text, Value, Text, Value)
    ann <- annFromJSON annJ
    Abs ann (Ident i) <$> exprFromJSON e

  appFromJSON = do
    (_, ann, e, e') <- parseJSON v :: Parser (Text, Value, Value, Value)
    App
      <$> annFromJSON ann
      <*> exprFromJSON e
      <*> exprFromJSON e'

  caseFromJSON = do
    (_, ann, ss, cs) <- parseJSON v :: Parser (Text, Value, Array, Array)
    Case
      <$> annFromJSON ann
      <*> parseArray exprFromJSON ss
      <*> parseArray caseAlternativeFromJSON cs

  letFromJSON = do
    (_, ann, bs, e) <- parseJSON v :: Parser (Text, Value, Array, Value)
    Let
      <$> annFromJSON ann
      <*> parseArray bindFromJSON bs
      <*> exprFromJSON e

caseAlternativeFromJSON :: Value -> Parser (CaseAlternative Ann)
caseAlternativeFromJSON v = do
  (bs, r) <- parseJSON v :: Parser ((Array, Value))
  CaseAlternative
    <$> parseArray binderFromJSON bs
    <*>
      (
            Left <$> withArray "parseResultWithGuards" (parseArray parseResultWithGuards) r
        <|> Right <$> exprFromJSON r
      )
  where
    parseResultWithGuards :: Value -> Parser (Guard Ann, Expr Ann)
    parseResultWithGuards =
      (parseJSON :: Value -> Parser (Value, Value))
      >=> traverseT . (exprFromJSON *** exprFromJSON)

    traverseT :: Applicative m => (m a, m b) -> m (a, b)
    traverseT (ma, mb) = (,) <$> ma <*> mb

binderFromJSON :: Value -> Parser (Binder Ann)
binderFromJSON v = do
  -- null binder is encoded as a String
  t <- tag v <|> Just <$> parseJSON v
  case t of
    Just "NullBinder"         -> modifyFailure ("binderFromJSON (NullBinder):" ++) nullBinderFromJSON
    Just "VarBinder"          -> modifyFailure ("binderFromJSON (VarBinder): " ++) varBinderFromJSON
    Just "LiteralBinder"      -> modifyFailure ("binderFromJSON (LiteralBinder): " ++) literalBinderFromJSON
    Just "ConstructorBinder"  -> modifyFailure ("binderFromJSON (ConstructorBinder): " ++) constructorBinderFromJSON
    Just "NamedBinder"        -> modifyFailure ("binderFromJSON (NamedBinder): " ++) namedBinderFromJSON
    Just s                    -> fail $ "binderFromJSON: not recognized binder: " ++ T.unpack s
    Nothing                   -> fail $ "binderFromJSON: error parsing binder: " ++ show v

  where
  nullBinderFromJSON = do
    (_, ann) <- parseJSON v :: Parser (Text, Value)
    NullBinder <$> annFromJSON ann

  varBinderFromJSON = do
    (_, annJ, i) <- parseJSON v :: Parser (Text, Value, Text)
    ann <- annFromJSON annJ
    return $ VarBinder ann (Ident i)

  literalBinderFromJSON = do
    (_, ann, l) <- parseJSON v :: Parser (Text, Value, Value)
    LiteralBinder <$> annFromJSON ann <*> literalFromJSON binderFromJSON l

  constructorBinderFromJSON = do
    (_, ann, d, c, bs) <- parseJSON v :: Parser (Text, Value, Value, Value, Array)
    ConstructorBinder
      <$> annFromJSON ann
      <*> qualifiedFromJSON ProperName d
      <*> qualifiedFromJSON ProperName c
      <*> parseArray binderFromJSON bs

  namedBinderFromJSON = do
    (_, annJ, n, b) <- parseJSON v :: Parser (Text, Value, Text, Value)
    ann <- annFromJSON annJ
    NamedBinder ann (Ident n) <$> binderFromJSON b
