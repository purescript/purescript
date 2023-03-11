-- |
-- Bundles compiled PureScript modules for the browser.
--
-- This module takes as input the individual generated modules from 'Language.PureScript.Make' and
-- performs dead code elimination, filters empty modules,
-- and generates the final JavaScript bundle.
module Language.PureScript.Bundle
  ( ModuleIdentifier(..)
  , ModuleType(..)
  , ErrorMessage(..)
  , printErrorMessage
  , ForeignModuleExports(..)
  , getExportedIdentifiers
  , ForeignModuleImports(..)
  , getImportedModules
  , Module
  ) where

import Prelude

import Control.Monad.Error.Class ( MonadError(throwError) )

import Data.Aeson ((.=))
import Data.Char (chr, digitToInt)
import Data.Foldable (fold)
import Data.Maybe (mapMaybe, maybeToList)
import Data.Aeson qualified as A
import Data.Text.Lazy qualified as LT

import Language.JavaScript.Parser
    ( JSStatement(JSAssignStatement, JSConstant, JSLet, JSClass,
                  JSGenerator, JSVariable, JSFunction),
      JSExpression(JSObjectLiteral, JSStringLiteral, JSMemberExpression,
                   JSVarInitExpression, JSFunctionExpression, JSMemberSquare,
                   JSMemberDot, JSIdentifier),
      JSAST(JSAstStatement, JSAstModule),
      renderToText,
      JSAnnot(JSNoAnnot),
      JSAssignOp(JSAssign) )
import Language.JavaScript.Parser.AST
    ( JSModuleItem(..),
      JSCommaList(..),
      JSPropertyName(JSPropertyIdent, JSPropertyString),
      JSIdent(JSIdentName),
      JSObjectPropertyList,
      JSCommaTrailingList(..),
      JSExportClause(JSExportClause),
      JSExportDeclaration(JSExport, JSExportFrom, JSExportLocals),
      JSExportSpecifier(JSExportSpecifierAs, JSExportSpecifier),
      JSFromClause(JSFromClause),
      JSImportDeclaration(JSImportDeclarationBare, JSImportDeclaration),
      JSObjectProperty(JSPropertyNameandValue),
      JSVarInitializer(JSVarInit) )
import Language.JavaScript.Process.Minify ( minifyJS )

-- | The type of error messages. We separate generation and rendering of errors using a data
-- type, in case we need to match on error types later.
data ErrorMessage
  = UnsupportedModulePath String
  | InvalidTopLevel
  | UnableToParseModule String
  | UnsupportedImport
  | UnsupportedExport
  | ErrorInModule ModuleIdentifier ErrorMessage
  | MissingEntryPoint String
  | MissingMainModule String
  deriving (Show)

-- | Modules are either "regular modules" (i.e. those generated by the PureScript compiler) or
-- foreign modules.
data ModuleType
  = Regular
  | Foreign
  deriving (Show, Eq, Ord)

showModuleType :: ModuleType -> String
showModuleType Regular = "Regular"
showModuleType Foreign = "Foreign"

-- | A module is identified by its module name and its type.
data ModuleIdentifier = ModuleIdentifier String ModuleType deriving (Show, Eq, Ord)

instance A.ToJSON ModuleIdentifier where
  toJSON (ModuleIdentifier name mt) =
    A.object [ "name" .= name
             , "type" .= show mt
             ]

data Visibility
  = Public
  | Internal
  deriving (Show, Eq, Ord)

-- | A piece of code is identified by its module, its name, and whether it is an internal variable
-- or a public member. These keys are used to label vertices in the dependency graph.
type Key = (ModuleIdentifier, String, Visibility)

-- | An export is either a "regular export", which exports a name from the regular module we are in,
-- or a reexport of a declaration in the corresponding foreign module.
--
-- Regular exports are labelled, since they might re-export an operator with another name.
data ExportType
  = RegularExport String
  | ForeignReexport
  deriving (Show, Eq, Ord)

-- | There are four types of module element we are interested in:
--
-- 1) Import declarations and require statements
-- 2) Member declarations
-- 3) Export lists
-- 4) Everything else
--
-- Each is labelled with the original AST node which generated it, so that we can dump it back
-- into the output during codegen.
data ModuleElement
  = Import JSModuleItem String (Either String ModuleIdentifier)
  | Member JSStatement Visibility String JSExpression [Key]
  | ExportsList [(ExportType, String, JSExpression, [Key])]
  | Other JSStatement
  | Skip JSModuleItem
  deriving (Show)

instance A.ToJSON ModuleElement where
  toJSON = \case
    (Import _ name (Right target)) ->
      A.object [ "type"   .= A.String "Import"
               , "name"   .= name
               , "target" .= target
               ]
    (Import _ name (Left targetPath)) ->
      A.object [ "type"       .= A.String "Import"
               , "name"       .= name
               , "targetPath" .= targetPath
               ]
    (Member _ visibility name _ dependsOn) ->
      A.object [ "type"       .= A.String "Member"
               , "name"       .= name
               , "visibility" .= show visibility
               , "dependsOn"  .= map keyToJSON dependsOn
               ]
    (ExportsList exports) ->
      A.object [ "type"    .= A.String "ExportsList"
               , "exports" .= map exportToJSON exports
               ]
    (Other stmt) ->
      A.object [ "type" .= A.String "Other"
               , "js"   .= getFragment (JSAstStatement stmt JSNoAnnot)
               ]
    (Skip item) ->
      A.object [ "type" .= A.String "Skip"
               , "js"   .= getFragment (JSAstModule [item] JSNoAnnot)
               ]

    where

    keyToJSON (mid, member, visibility) =
      A.object [ "module"     .= mid
               , "member"     .= member
               , "visibility" .= show visibility
               ]

    exportToJSON (RegularExport sourceName, name, _, dependsOn) =
      A.object [ "type"       .= A.String "RegularExport"
               , "name"       .= name
               , "sourceName" .= sourceName
               , "dependsOn"  .= map keyToJSON dependsOn
               ]
    exportToJSON (ForeignReexport, name, _, dependsOn) =
      A.object [ "type"      .= A.String "ForeignReexport"
               , "name"      .= name
               , "dependsOn" .= map keyToJSON dependsOn
               ]

    getFragment = ellipsize . renderToText . minifyJS
      where
      ellipsize text = if LT.compareLength text 20 == GT then LT.take 19 text `LT.snoc` ellipsis else text
      ellipsis = '\x2026'

-- | A module is just a list of elements of the types listed above.
data Module = Module ModuleIdentifier (Maybe FilePath) [ModuleElement] deriving (Show)

instance A.ToJSON Module where
  toJSON (Module moduleId filePath elements) =
    A.object [ "moduleId" .= moduleId
             , "filePath" .= filePath
             , "elements" .= elements
             ]

-- | Prepare an error message for consumption by humans.
printErrorMessage :: ErrorMessage -> [String]
printErrorMessage (UnsupportedModulePath s) =
  [ "An ES or CommonJS module has an unsupported name (" ++ show s ++ ")."
  , "The following file names are supported:"
  , "  1) index.js (PureScript native modules)"
  , "  2) foreign.js (PureScript ES foreign modules)"
  , "  3) foreign.cjs (PureScript CommonJS foreign modules)"
  ]
printErrorMessage InvalidTopLevel =
  [ "Expected a list of source elements at the top level." ]
printErrorMessage (UnableToParseModule err) =
  [ "The module could not be parsed:"
  , err
  ]
printErrorMessage UnsupportedImport =
  [ "An import was unsupported."
  , "Modules can be imported with ES namespace imports declarations:"
  , "  import * as module from \"Module.Name\""
  , "Alternatively, they can be also be imported with the CommonJS require function:"
  , "  var module = require(\"Module.Name\")"
  ]
printErrorMessage UnsupportedExport =
  [ "An export was unsupported."
  , "Declarations can be exported as ES named exports:"
  , "  export var decl"
  , "Existing identifiers can be exported as well:"
  , "  export { name }"
  , "They can also be renamed on export:"
  , "  export { name as alias }"
  , "Alternatively, CommonJS exports can be defined in one of two ways:"
  , "  1) exports.name = value"
  , "  2) exports = { name: value }"
  ]
printErrorMessage (ErrorInModule mid e) =
  ("Error in module " ++ displayIdentifier mid ++ ":")
  : ""
  : map ("  " ++) (printErrorMessage e)
  where
    displayIdentifier (ModuleIdentifier name ty) =
      name ++ " (" ++ showModuleType ty ++ ")"
printErrorMessage (MissingEntryPoint mName) =
  [ "Could not find an ES module or CommonJS module for the specified entry point: " ++ mName
  ]
printErrorMessage (MissingMainModule mName) =
  [ "Could not find an ES module or CommonJS module for the specified main module: " ++ mName
  ]

-- String literals include the quote chars
fromStringLiteral :: JSExpression -> Maybe String
fromStringLiteral (JSStringLiteral _ str) = Just $ strValue str
fromStringLiteral _ = Nothing

strValue :: String -> String
strValue str = go $ drop 1 str
  where
  go ('\\' : 'b' : xs) = '\b' : go xs
  go ('\\' : 'f' : xs) = '\f' : go xs
  go ('\\' : 'n' : xs) = '\n' : go xs
  go ('\\' : 'r' : xs) = '\r' : go xs
  go ('\\' : 't' : xs) = '\t' : go xs
  go ('\\' : 'v' : xs) = '\v' : go xs
  go ('\\' : '0' : xs) = '\0' : go xs
  go ('\\' : 'x' : a : b : xs) = chr (a' + b') : go xs
    where
    a' = 16 * digitToInt a
    b' = digitToInt b
  go ('\\' : 'u' : a : b : c : d : xs) = chr (a' + b' + c' + d') : go xs
    where
    a' = 16 * 16 * 16 * digitToInt a
    b' = 16 * 16 * digitToInt b
    c' = 16 * digitToInt c
    d' = digitToInt d
  go ('\\' : x : xs) = x : go xs
  go "\"" = ""
  go "'" = ""
  go (x : xs) = x : go xs
  go "" = ""

commaList :: JSCommaList a -> [a]
commaList JSLNil = []
commaList (JSLOne x) = [x]
commaList (JSLCons l _ x) = commaList l ++ [x]

trailingCommaList :: JSCommaTrailingList a -> [a]
trailingCommaList (JSCTLComma l _) = commaList l
trailingCommaList (JSCTLNone l) = commaList l

identName :: JSIdent -> Maybe String
identName (JSIdentName _ ident) = Just ident
identName _ = Nothing

exportStatementIdentifiers :: JSStatement -> [String]
exportStatementIdentifiers (JSVariable _ jsExpressions _) =
  varNames jsExpressions
exportStatementIdentifiers (JSConstant _ jsExpressions _) =
  varNames jsExpressions
exportStatementIdentifiers (JSLet _ jsExpressions _) =
  varNames jsExpressions
exportStatementIdentifiers (JSClass _ jsIdent _ _ _ _ _) =
  maybeToList . identName $ jsIdent
exportStatementIdentifiers (JSFunction _ jsIdent _ _ _ _ _) =
  maybeToList . identName $ jsIdent
exportStatementIdentifiers (JSGenerator _ _ jsIdent _ _ _ _ _) =
  maybeToList . identName $ jsIdent
exportStatementIdentifiers _ = []

varNames :: JSCommaList JSExpression -> [String]
varNames = mapMaybe varName . commaList
  where
  varName (JSVarInitExpression (JSIdentifier _ ident) _) = Just ident
  varName _ = Nothing

data ForeignModuleExports =
  ForeignModuleExports
    { cjsExports :: [String]
    , esExports :: [String]
    } deriving (Show)

instance Semigroup ForeignModuleExports where
  (ForeignModuleExports cjsExports esExports) <> (ForeignModuleExports cjsExports' esExports') =
    ForeignModuleExports (cjsExports <> cjsExports') (esExports <> esExports')
instance Monoid ForeignModuleExports where
  mempty = ForeignModuleExports [] []

-- Get a list of all the exported identifiers from a foreign module.
--
-- TODO: what if we assign to exports.foo and then later assign to
-- module.exports (presumably overwriting exports.foo)?
getExportedIdentifiers :: forall m. (MonadError ErrorMessage m)
                          => String
                          -> JSAST
                          -> m ForeignModuleExports
getExportedIdentifiers mname top
  | JSAstModule jsModuleItems _ <- top = fold <$> traverse go jsModuleItems
  | otherwise = err InvalidTopLevel
  where
  err :: ErrorMessage -> m a
  err = throwError . ErrorInModule (ModuleIdentifier mname Foreign)

  go (JSModuleStatementListItem jsStatement)
    | Just props <- matchExportsAssignment jsStatement
    = do cjsExports <- traverse toIdent (trailingCommaList props)
         pure ForeignModuleExports{ cjsExports, esExports = [] }
    | Just (Public, name, _) <- matchMember jsStatement
    = pure ForeignModuleExports{ cjsExports = [name], esExports = [] }
    | otherwise
    = pure mempty
  go (JSModuleExportDeclaration _ jsExportDeclaration) =
    pure ForeignModuleExports{ cjsExports = [], esExports = exportDeclarationIdentifiers jsExportDeclaration }
  go _ = pure mempty

  toIdent (JSPropertyNameandValue name _ [_]) =
    extractLabel' name
  toIdent _ =
    err UnsupportedExport

  extractLabel' = maybe (err UnsupportedExport) pure . extractLabel

  exportDeclarationIdentifiers (JSExportFrom jsExportClause _ _) =
    exportClauseIdentifiers jsExportClause
  exportDeclarationIdentifiers (JSExportLocals jsExportClause _) =
    exportClauseIdentifiers jsExportClause
  exportDeclarationIdentifiers (JSExport jsStatement _) =
    exportStatementIdentifiers jsStatement

  exportClauseIdentifiers (JSExportClause _ jsExportsSpecifiers _) =
    mapMaybe exportSpecifierName $ commaList jsExportsSpecifiers

  exportSpecifierName (JSExportSpecifier jsIdent) = identName jsIdent
  exportSpecifierName (JSExportSpecifierAs _ _ jsIdentAs) = identName jsIdentAs

data ForeignModuleImports =
  ForeignModuleImports
    { cjsImports :: [String]
    , esImports :: [String]
    } deriving (Show)

instance Semigroup ForeignModuleImports where
  (ForeignModuleImports cjsImports esImports) <> (ForeignModuleImports cjsImports' esImports') =
    ForeignModuleImports (cjsImports <> cjsImports') (esImports <> esImports')
instance Monoid ForeignModuleImports where
  mempty = ForeignModuleImports [] []

-- Get a list of all the imported module identifiers from a foreign module.
getImportedModules :: forall m. (MonadError ErrorMessage m)
                          => String
                          -> JSAST
                          -> m ForeignModuleImports
getImportedModules mname top
  | JSAstModule jsModuleItems _ <- top = pure $ foldMap go jsModuleItems
  | otherwise = err InvalidTopLevel
  where
  err :: ErrorMessage -> m a
  err = throwError . ErrorInModule (ModuleIdentifier mname Foreign)

  go (JSModuleStatementListItem jsStatement)
    | Just (_, mid) <- matchRequire jsStatement
    = ForeignModuleImports{ cjsImports = [mid], esImports = [] }
  go (JSModuleImportDeclaration _ jsImportDeclaration) =
    ForeignModuleImports{ cjsImports = [], esImports = [importDeclarationModuleId jsImportDeclaration] }
  go _ = mempty

  importDeclarationModuleId (JSImportDeclaration _ (JSFromClause _ _ mid) _) = mid
  importDeclarationModuleId (JSImportDeclarationBare _ mid _) = mid

-- Matches JS statements like this:
-- var ModuleName = require("file");
matchRequire :: JSStatement -> Maybe (String, String)
matchRequire stmt
  | JSVariable _ jsInit _ <- stmt
  , [JSVarInitExpression var varInit] <- commaList jsInit
  , JSIdentifier _ importName <- var
  , JSVarInit _ jsInitEx <- varInit
  , JSMemberExpression req _ argsE _ <- jsInitEx
  , JSIdentifier _ "require" <- req
  , [ Just importPath ] <- map fromStringLiteral (commaList argsE)
  = Just (importName, importPath)
  | otherwise
  = Nothing

-- Matches JS member declarations.
matchMember :: JSStatement -> Maybe (Visibility, String, JSExpression)
matchMember stmt
  | Just (name, decl) <- matchInternalMember stmt
  = pure (Internal, name, decl)
  -- exports.foo = expr; exports["foo"] = expr;
  | JSAssignStatement e (JSAssign _) decl _ <- stmt
  , Just name <- exportsAccessor e
  = Just (Public, name, decl)
  | otherwise
  = Nothing

matchInternalMember :: JSStatement -> Maybe (String, JSExpression)
matchInternalMember stmt
  -- var foo = expr;
  | JSVariable _ jsInit _ <- stmt
  , [JSVarInitExpression var varInit] <- commaList jsInit
  , JSIdentifier _ name <- var
  , JSVarInit _ decl <- varInit
  = pure (name, decl)
  -- function foo(...args) { body }
  | JSFunction a0 jsIdent a1 args a2 body _ <- stmt
  , JSIdentName _ name <- jsIdent
  = pure (name, JSFunctionExpression a0 jsIdent a1 args a2 body)
  | otherwise
  = Nothing

-- Matches exports.* or exports["*"] expressions and returns the property name.
exportsAccessor :: JSExpression -> Maybe String
exportsAccessor (JSMemberDot exports _ nm)
  | JSIdentifier _ "exports" <- exports
  , JSIdentifier _ name <- nm
  = Just name
exportsAccessor (JSMemberSquare exports _ nm _)
  | JSIdentifier _ "exports" <- exports
  , Just name <- fromStringLiteral nm
  = Just name
exportsAccessor _ = Nothing

-- Matches assignments to module.exports, like this:
-- module.exports = { ... }
matchExportsAssignment :: JSStatement -> Maybe JSObjectPropertyList
matchExportsAssignment stmt
  | JSAssignStatement e (JSAssign _) decl _ <- stmt
  , JSMemberDot module' _ exports <- e
  , JSIdentifier _ "module" <- module'
  , JSIdentifier _ "exports" <- exports
  , JSObjectLiteral _ props _ <- decl
  = Just props
  | otherwise
  = Nothing

extractLabel :: JSPropertyName -> Maybe String
extractLabel (JSPropertyString _ nm) = Just $ strValue nm
extractLabel (JSPropertyIdent _ nm) = Just nm
extractLabel _ = Nothing
