-- |
-- Read the core functional representation from JSON format
--

module Language.PureScript.CoreFn.FromJSON
  ( moduleFromJSON
  ) where

import Prelude.Compat

import           Data.Aeson
import           Data.Aeson.Types (Parser, Value, listParser)
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.ParserCombinators.ReadP (readP_to_S)
import qualified Data.Vector as V
import           Data.Version (Version, parseVersion)

import           Language.PureScript.AST.SourcePos ()
import           Language.PureScript.AST.Literals
import           Language.PureScript.CoreFn.Ann
import           Language.PureScript.CoreFn
import           Language.PureScript.Names
import           Language.PureScript.PSString (PSString)

constructorTypeFromJSON :: Value -> Parser ConstructorType
constructorTypeFromJSON v = do
  t <- parseJSON v
  case t of
    "ProductType" -> return ProductType
    "SumType"     -> return SumType
    _             -> fail ("not regonized ConstructorType: " ++ T.unpack t)

metaFromJSON :: Value -> Parser (Maybe Meta)
metaFromJSON Null = return Nothing
metaFromJSON v = withObject "Meta" metaFromObj v
  where
    metaFromObj o = do
      type_ <- o .: "metaType"
      case type_ of
        "IsConstructor" -> isConstructorFromJSON o
        "IsNewtype"     -> return $ Just IsNewtype
        "IsTypeClassConstructor"
                        -> return $ Just IsTypeClassConstructor
        "IsForeign"     -> return $ Just IsForeign
        _               -> fail ("not recognized Meta: " ++ T.unpack type_)

    isConstructorFromJSON o = do
      ct <- o .: "constructorType" >>= constructorTypeFromJSON
      is <- o .: "identifiers" >>= listParser identFromJSON
      return $ Just (IsConstructor ct is)

annFromJSON :: Value -> Parser Ann
annFromJSON = withObject "Ann" annFromObj
  where
  annFromObj o = do
    ss <- o .: "sourceSpan"
    mm <- o .: "meta" >>= metaFromJSON
    return (ss, [], Nothing, mm)

literalFromJSON :: (Value -> Parser a) -> Value -> Parser (Literal a)
literalFromJSON t = withObject "Literal" literalFromObj
  where
  literalFromObj o = do
    type_ <- o .: "literalType" :: Parser Text
    case type_ of
      "IntLiteral"      -> NumericLiteral . Left <$> o .: "value"
      "NumberLiteral"   -> NumericLiteral . Right <$> o .: "value"
      "StringLiteral"   -> StringLiteral <$> o .: "value"
      "CharLiteral"     -> CharLiteral <$> o .: "value"
      "BooleanLiteral"  -> BooleanLiteral <$> o .: "value"
      "ArrayLiteral"    -> parseArrayLiteral o
      "ObjectLiteral"   -> parseObjectLiteral o
      _                 -> fail ("error parsing Literal: " ++ show o)

  parseArrayLiteral o = do
    val <- o .: "value"
    as <- mapM t (V.toList val)
    return $ ArrayLiteral as

  parseObjectLiteral o = do
    val <- o .: "value"
    ObjectLiteral <$> recordFromJSON t val

identFromJSON :: Value -> Parser Ident
identFromJSON = withText "Ident" (return . Ident)

properNameFromJSON :: Value -> Parser (ProperName a)
properNameFromJSON = fmap ProperName . parseJSON

qualifiedFromJSON :: (Text -> a) -> Value -> Parser (Qualified a)
qualifiedFromJSON f = withObject "Qualified" qualifiedFromObj
  where
  qualifiedFromObj o = do
    mn <- o .:? "moduleName" >>= sequence . fmap moduleNameFromJSON
    i  <- o .: "identifier" >>= withText "Ident" (return . f)
    return $ Qualified mn i

moduleNameFromJSON :: Value -> Parser ModuleName
moduleNameFromJSON v = ModuleName <$> listParser properNameFromJSON v

moduleFromJSON :: Value -> Parser (Version, ModuleT () Ann)
moduleFromJSON = withObject "Module" moduleFromObj
  where
  moduleFromObj o = do
    version <- o .: "builtWith" >>= versionFromJSON
    moduleName <- o .: "moduleName" >>= moduleNameFromJSON
    moduleImports <- o .: "imports" >>= listParser importFromJSON
    moduleExports <- o .: "exports" >>= listParser identFromJSON
    moduleDecls <- o .: "decls" >>= listParser bindFromJSON
    moduleForeign <- o .: "foreign" >>= listParser (fmap (flip (,) ()) . identFromJSON)
    -- moduleComments are not in the CoreFn json representation
    let moduleComments = []
    return (version, Module {..})

  versionFromJSON :: String -> Parser Version
  versionFromJSON v =
    case readP_to_S parseVersion v of
      (r, _) : _ -> return r
      _ -> fail "failed parsing purs version"

  importFromJSON :: Value -> Parser (Ann, ModuleName)
  importFromJSON = withObject "Import"
    (\o -> do
      ann <- o .: "annotation" >>= annFromJSON
      mn  <- o .: "moduleName" >>= moduleNameFromJSON
      return (ann, mn))

bindFromJSON :: Value -> Parser (Bind Ann)
bindFromJSON = withObject "Bind" bindFromObj
  where
  bindFromObj :: Object -> Parser (Bind Ann)
  bindFromObj o = do
    type_ <- o .: "bindType" :: Parser Text
    case type_ of
      "NonRec"  -> (uncurry . uncurry $ NonRec) <$> bindFromObj' o
      "Rec"     -> Rec <$> (o .: "binds" >>= listParser (withObject "Bind" bindFromObj'))
      _         -> fail ("not recognized bind type \"" ++ T.unpack type_ ++ "\"")
        
  bindFromObj' :: Object -> Parser ((Ann, Ident), Expr Ann)
  bindFromObj' o = do
    a <- o .: "annotation" >>= annFromJSON
    i <- o .: "identifier" >>= identFromJSON
    e <- o .: "expression" >>= exprFromJSON
    return ((a, i), e)

recordFromJSON :: (Value -> Parser a) -> Value -> Parser [(PSString, a)]
recordFromJSON p = listParser parsePair
  where
  parsePair v = do
    (l, v') <- parseJSON v :: Parser (PSString, Value)
    a <- p v'
    return (l, a)

exprFromJSON :: Value -> Parser (Expr Ann)
exprFromJSON = withObject "Expr" exprFromObj
  where
  exprFromObj o = do
    type_ <- o .: "type"
    case type_ of
      "Var"           -> varFromObj o
      "Literal"       -> literalExprFromObj o
      "Constructor"   -> constructorFromObj o
      "Accessor"      -> accessorFromObj o
      "ObjectUpdate"  -> objectUpdateFromObj o
      "Abs"           -> absFromObj o
      "App"           -> appFromObj o
      "Case"          -> caseFromObj o
      "Let"           -> letFromObj o
      _               -> fail ("not recognized expression type: \"" ++ T.unpack type_ ++ "\"")

  varFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON
    qi <- o .: "value" >>= qualifiedFromJSON Ident
    return $ Var ann qi

  literalExprFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON
    lit <- o .: "value" >>= literalFromJSON exprFromJSON
    return $ Literal ann lit

  constructorFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON
    tyn <- o .: "typeName" >>= properNameFromJSON
    con <- o .: "constructorName" >>= properNameFromJSON
    is  <- o .: "fieldNames" >>= listParser identFromJSON
    return $ Constructor ann tyn con is

  accessorFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON
    f   <- o .: "fieldName"
    e <- o .: "expression" >>= exprFromJSON
    return $ Accessor ann f e

  objectUpdateFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON
    e   <- o .: "expression" >>= exprFromJSON
    us  <- o .: "updates" >>= recordFromJSON exprFromJSON
    return $ ObjectUpdate ann e us

  absFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON
    idn <- o .: "argument" >>= identFromJSON
    e   <- o .: "body" >>= exprFromJSON
    return $ Abs ann idn e

  appFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON
    e   <- o .: "abstraction" >>= exprFromJSON
    e'  <- o .: "argument" >>= exprFromJSON
    return $ App ann e e'

  caseFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON
    cs  <- o .: "caseExpressions" >>= listParser exprFromJSON
    cas <- o .: "caseAlternatives" >>= listParser caseAlternativeFromJSON
    return $ Case ann cs cas

  letFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON
    bs  <- o .: "binds" >>= listParser bindFromJSON
    e   <- o .: "expression" >>= exprFromJSON
    return $ Let ann bs e

caseAlternativeFromJSON :: Value -> Parser (CaseAlternative Ann)
caseAlternativeFromJSON = withObject "CaseAlternative" caseAlternativeFromObj
  where
    caseAlternativeFromObj o = do
      bs <- o .: "binders" >>= listParser binderFromJSON
      isGuarded <- o .: "isGuarded"
      if isGuarded
        then do
          es <- o .: "expressions" >>= listParser parseResultWithGuard
          return $ CaseAlternative bs (Left es)
        else do
          e <- o .: "expression" >>= exprFromJSON
          return $ CaseAlternative bs (Right e)

    parseResultWithGuard :: Value -> Parser (Guard Ann, Expr Ann)
    parseResultWithGuard = withObject "parseCaseWithGuards" $
      \o -> do
        g <- o .: "guard" >>= exprFromJSON
        e <- o .: "expression" >>= exprFromJSON
        return (g, e)

binderFromJSON :: Value -> Parser (Binder Ann)
binderFromJSON = withObject "Binder" binderFromObj
  where
  binderFromObj o = do
    type_ <- o .: "binderType"
    case type_ of
      "NullBinder"        -> nullBinderFromObj o
      "VarBinder"         -> varBinderFromObj o
      "LiteralBinder"     -> literalBinderFromObj o
      "ConstructorBinder" -> constructorBinderFromObj o
      "NamedBinder"       -> namedBinderFromObj o
      _                   -> fail ("not recognized binder: \"" ++ T.unpack type_ ++ "\"")


  nullBinderFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON
    return $ NullBinder ann

  varBinderFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON
    idn <- o .: "identifier" >>= identFromJSON
    return $ VarBinder ann idn

  literalBinderFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON
    lit <- o .: "literal" >>= literalFromJSON binderFromJSON
    return $ LiteralBinder ann lit

  constructorBinderFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON
    tyn <- o .: "typeName" >>= qualifiedFromJSON ProperName
    con <- o .: "constructorName" >>= qualifiedFromJSON ProperName
    bs  <- o .: "binders" >>= listParser binderFromJSON
    return $ ConstructorBinder ann tyn con bs

  namedBinderFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON
    n   <- o .: "identifier" >>= identFromJSON
    b   <- o .: "binder" >>= binderFromJSON
    return $ NamedBinder ann n b
