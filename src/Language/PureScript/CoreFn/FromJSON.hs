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

import Language.PureScript.AST.Literals
import Language.PureScript.CoreFn
import Language.PureScript.Names
import Language.PureScript.PSString (PSString, mkString)

parseArray :: (Value -> Parser a) -> Array -> Parser [a]
parseArray p = sequence . foldr (\a as -> p a : as) []

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

moduleFromJSON :: Value -> Parser (Version, ModuleT () ())
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

  moduleFromJSON' :: ModuleName -> Value -> Parser (ModuleT () ())
  moduleFromJSON' moduleName v = do
    o <- parseJSON v
    moduleImports <- fmap (((),) . moduleNameFromString) <$> o .: "imports"
    me <- o .: "exports"
    let moduleExports = Ident <$> me
    moduleDecls <- o .: "decls"
      >>= withArray "declarations" (parseArray bindFromJSON)
    moduleForeign <- o .: "foreign"
      >>= withArray "foreign" (parseArray (fmap (flip (,) ()) . parseIdent))
    -- moduleComments are not in the CoreFn json representation
    let moduleComments = []
    return $ Module {..}

bindFromJSON :: Value -> Parser (Bind ())
bindFromJSON = withObject "Bind" parseBind
  where
  parseBind o = do
    o' <- traverse exprFromJSON o
    let l = HM.foldrWithKey (\k e t -> (((), Ident k), e) : t) [] o'
    case l of
      ((_, i), e) : []  -> pure (NonRec () i e)
      _                 -> pure $ Rec l

tag :: Value -> Parser (Maybe Text)
tag = withArray "tag" (sequence . fmap parseJSON . (!? 0))

exprFromJSON :: Value -> Parser (Expr ())
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
    (_, i) <- parseJSON v :: Parser (Text, Value)
    Var () <$> qualifiedFromJSON Ident i

  literalExprFromJSON = do
    (_, l) <- parseJSON v :: Parser (Text, Value)
    Literal () <$> literalFromJSON exprFromJSON l

  constructorFromJSON = do
    (_, d, c, is) <- parseJSON v :: Parser (Text, Value, Value, [Text])
    Constructor () <$> properNameFromJSON d <*> properNameFromJSON c <*> return (Ident <$> is)

  accessorFromJSON = do
    (_, t, e) <- parseJSON v :: Parser (Text, PSString, Value)
    Accessor () t <$> exprFromJSON e

  objectUpdateFromJSON = do
    (_, r, fs) <- parseJSON v :: Parser (Text, Value, Object)
    ObjectUpdate ()
      <$> exprFromJSON r
      <*> for (HM.toList fs) (sequence . (mkString *** exprFromJSON))

  absFromJSON = do
    (_, i, e) <- parseJSON v :: Parser (Text, Text, Value)
    Abs () (Ident i) <$> exprFromJSON e

  appFromJSON = do
    (_, e, e') <- parseJSON v :: Parser (Text, Value, Value)
    App () <$> exprFromJSON e <*> exprFromJSON e'

  caseFromJSON = do
    (_, ss, cs) <- parseJSON v :: Parser (Text, Array, Array)
    _ <- parseArray exprFromJSON ss
    Case () <$> parseArray exprFromJSON ss <*> parseArray caseAlternativeFromJSON cs

  letFromJSON = do
    (_, bs, e) <- parseJSON v :: Parser (Text, Array, Value)
    Let () <$> parseArray bindFromJSON bs <*> exprFromJSON e

caseAlternativeFromJSON :: Value -> Parser (CaseAlternative ())
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
    parseResultWithGuards :: Value -> Parser (Guard (), Expr ())
    parseResultWithGuards =
      (parseJSON :: Value -> Parser (Value, Value))
      >=> traverseT . (exprFromJSON *** exprFromJSON)

    traverseT :: Applicative m => (m a, m b) -> m (a, b)
    traverseT (ma, mb) = (,) <$> ma <*> mb
      
binderFromJSON :: Value -> Parser (Binder ())
binderFromJSON v = do
  -- null binder is encoded as a String
  t <- tag v <|> Just <$> parseJSON v
  case t of
    Just "NullBinder"         -> return $ NullBinder ()
    Just "VarBinder"          -> modifyFailure ("binderFromJSON (VarBinder): " ++) varBinderFromJSON
    Just "LiteralBinder"      -> modifyFailure ("binderFromJSON (LiteralBinder): " ++) literalBinderFromJSON
    Just "ConstructorBinder"  -> modifyFailure ("binderFromJSON (ConstructorBinder): " ++) constructorBinderFromJSON
    Just "NamedBinder"        -> modifyFailure ("binderFromJSON (NamedBinder): " ++) namedBinderFromJSON
    Just s                    -> fail $ "binderFromJSON: not recognized binder: " ++ T.unpack s
    Nothing                   -> fail $ "binderFromJSON: error parsing binder: " ++ show v

  where
  varBinderFromJSON = do
    (_, i) <- parseJSON v :: Parser (Text, Text)
    return $ VarBinder () (Ident i)

  literalBinderFromJSON = do
    (_, l) <- parseJSON v :: Parser (Text, Value)
    LiteralBinder () <$> literalFromJSON binderFromJSON l

  constructorBinderFromJSON = do
    (_, d, c, bs) <- parseJSON v :: Parser (Text, Value, Value, Array)
    ConstructorBinder ()
      <$> qualifiedFromJSON ProperName d
      <*> qualifiedFromJSON ProperName c
      <*> parseArray binderFromJSON bs

  namedBinderFromJSON = do
    (_, n, b) <- parseJSON v :: Parser (Text, Text, Value)
    NamedBinder () (Ident n) <$> binderFromJSON b
