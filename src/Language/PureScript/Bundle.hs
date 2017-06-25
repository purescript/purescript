-----------------------------------------------------------------------------
--
-- Module      :  psc-bundle
-- Copyright   :  (c) Phil Freeman 2015
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- Bundles compiled PureScript modules for the browser.
--
-- This module takes as input the individual generated modules from 'Language.PureScript.Make' and
-- performs dead code elimination, filters empty modules,
-- and generates the final Javascript bundle.
-----------------------------------------------------------------------------
module Language.PureScript.Bundle
  ( bundle
  , bundleSM
  , guessModuleIdentifier
  , ModuleIdentifier(..)
  , moduleName
  , ModuleType(..)
  , ErrorMessage(..)
  , printErrorMessage
  , getExportedIdentifiers
  ) where

import Prelude.Compat
import Protolude (ordNub)
import Control.Monad
import Control.Monad.Error.Class
import Control.Arrow ((&&&))
import Data.Char (chr, digitToInt)
import Data.Generics (everything, everywhere, mkQ, mkT)
import Data.Graph
import Data.List (stripPrefix)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Generics (everything, everywhere, mkQ, mkT)
import Data.Graph
import Data.Version (showVersion)

import qualified Data.Set as S

import Language.JavaScript.Parser.AST hiding (showStripped)
import Language.JavaScript.Parser
import Language.PureScript.Bundle.BundleOpt
import Language.PureScript.Bundle.BundleTypes

-- import Debug.Trace

import qualified Paths_purescript as Paths

import System.FilePath (takeFileName, takeDirectory, makeRelative)

import SourceMap.Types

-- | The type of error messages. We separate generation and rendering of errors using a data
-- type, in case we need to match on error types later.
data ErrorMessage
  = UnsupportedModulePath String
  | InvalidTopLevel
  | UnableToParseModule String
  | UnsupportedExport
  | ErrorInModule ModuleIdentifier ErrorMessage
  | MissingEntryPoint String
  | MissingMainModule String
  deriving (Show)

-- | Given a filename, assuming it is in the correct place on disk, infer a ModuleIdentifier.
guessModuleIdentifier :: MonadError ErrorMessage m => FilePath -> m ModuleIdentifier
guessModuleIdentifier filename = ModuleIdentifier (takeFileName (takeDirectory filename)) <$> guessModuleType (takeFileName filename)
  where
    guessModuleType "index.js" = pure Regular
    guessModuleType "foreign.js" = pure Foreign
    guessModuleType name = throwError $ UnsupportedModulePath name

-- | Prepare an error message for consumption by humans.
printErrorMessage :: ErrorMessage -> [String]
printErrorMessage (UnsupportedModulePath s) =
  [ "A CommonJS module has an unsupported name (" ++ show s ++ ")."
  , "The following file names are supported:"
  , "  1) index.js (PureScript native modules)"
  , "  2) foreign.js (PureScript foreign modules)"
  ]
printErrorMessage InvalidTopLevel =
  [ "Expected a list of source elements at the top level." ]
printErrorMessage (UnableToParseModule err) =
  [ "The module could not be parsed:"
  , err
  ]
printErrorMessage UnsupportedExport =
  [ "An export was unsupported. Exports can be defined in one of two ways: "
  , "  1) exports.name = ..."
  , "  2) exports = { ... }"
  ]
printErrorMessage (ErrorInModule mid e) =
  ("Error in module " ++ displayIdentifier mid ++ ":")
  : ""
  : map ("  " ++) (printErrorMessage e)
  where
    displayIdentifier (ModuleIdentifier name ty) =
      name ++ " (" ++ showModuleType ty ++ ")"
printErrorMessage (MissingEntryPoint mName) =
  [ "Couldn't find a CommonJS module for the specified entry point: " ++ mName
  ]
printErrorMessage (MissingMainModule mName) =
  [ "Couldn't find a CommonJS module for the specified main module: " ++ mName
  ]

-- | Calculate the ModuleIdentifier which a require(...) statement imports.
checkImportPath :: String -> ModuleIdentifier -> S.Set String -> Either String ModuleIdentifier
checkImportPath "./foreign" m _ =
  Right (ModuleIdentifier (moduleName m) Foreign)
checkImportPath name _ names
  | Just name' <- stripPrefix "../" name
  , name' `S.member` names = Right (ModuleIdentifier name' Regular)
checkImportPath name _ _ = Left name

-- | Compute the dependencies of all elements in a module, and add them to the tree.
--
-- Members and exports can have dependencies. A dependency is of one of the following forms:
--
-- 1) module.name or member["name"]
--
--    where module was imported using
--
--    var module = require("Module.Name");
--
-- 2) name
--
--    where name is the name of a member defined in the current module.
withDeps :: Module -> Module
withDeps (Module modulePath fn es) = Module modulePath fn (map expandDeps es)
  where
  -- | Collects all modules which are imported, so that we can identify dependencies of the first type.
  imports :: [(String, ModuleIdentifier)]
  imports = mapMaybe toImport es
    where
    toImport :: ModuleElement -> Maybe (String, ModuleIdentifier)
    toImport (Require _ nm (Right mid)) = Just (nm, mid)
    toImport _ = Nothing

  -- | Collects all member names in scope, so that we can identify dependencies of the second type.
  boundNames :: [String]
  boundNames = mapMaybe toBoundName es
    where
    toBoundName :: ModuleElement -> Maybe String
    toBoundName (Member _ _ nm _ _) = Just nm
    toBoundName _ = Nothing

  -- | Calculate dependencies and add them to the current element.
  expandDeps :: ModuleElement -> ModuleElement
  expandDeps (Member n f nm decl _) = Member n f nm decl (ordNub $ dependencies modulePath decl)
  expandDeps (ExportsList exps) = ExportsList (map expand exps)
      where
      expand (ty, nm, n1, _) = (ty, nm, n1, ordNub (dependencies modulePath n1))
  expandDeps other = other

  dependencies :: ModuleIdentifier -> JSExpression -> [(ModuleIdentifier, String)]
  dependencies m = everything (++) (mkQ [] toReference)
    where
    toReference :: JSExpression -> [(ModuleIdentifier, String)]
    toReference (JSMemberDot mn _ nm)
      | JSIdentifier _ mn' <- mn
      , JSIdentifier _ nm' <- nm
      , Just mid <- lookup mn' imports
      = [(mid, nm')]
    toReference (JSMemberSquare mn _ nm _)
      | JSIdentifier _ mn' <- mn
      , Just nm' <- fromStringLiteral nm
      , Just mid <- lookup mn' imports
      = [(mid, nm')]
    toReference (JSIdentifier _ nm)
      | nm `elem` boundNames
      = [(m, nm)]
    toReference _ = []

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

-- | Attempt to create a Module from a JavaScript AST.
--
-- Each type of module element is matched using pattern guards, and everything else is bundled into the
-- Other constructor.
toModule :: forall m. (MonadError ErrorMessage m) => S.Set String -> ModuleIdentifier -> Maybe FilePath -> JSAST -> m Module
toModule mids mid filename top
  | JSAstProgram smts _ <- top = Module mid filename <$> traverse toModuleElement smts
  | otherwise = err InvalidTopLevel
  where
  err = throwError . ErrorInModule mid

  toModuleElement :: JSStatement -> m ModuleElement
  toModuleElement stmt
    | Just (importName, importPath) <- matchRequire mids mid stmt
    = pure (Require stmt importName importPath)
  toModuleElement stmt
    | Just (exported, name, decl) <- matchMember stmt
    = pure (Member stmt exported name decl [])
  toModuleElement stmt
    | Just props <- matchExportsAssignment stmt
    = ExportsList <$> traverse toExport (trailingCommaList props)
    where
      toExport :: JSObjectProperty -> m (ExportType, String, JSExpression, [Key])
      toExport (JSPropertyNameandValue name _ [val]) =
        (,,val,[]) <$> exportType val
                   <*> extractLabel' name
      toExport _ = err UnsupportedExport

      exportType :: JSExpression -> m ExportType
      exportType (JSMemberDot f _ _)
        | JSIdentifier _ "$foreign" <- f
        = pure ForeignReexport
      exportType (JSMemberSquare f _ _ _)
        | JSIdentifier _ "$foreign" <- f
        = pure ForeignReexport
      exportType (JSIdentifier _ s) = pure (RegularExport s)
      exportType _ = err UnsupportedExport

      extractLabel' = maybe (err UnsupportedExport) pure . extractLabel

  toModuleElement other = pure (Other other)

-- Get a list of all the exported identifiers from a foreign module.
--
-- TODO: what if we assign to exports.foo and then later assign to
-- module.exports (presumably overwriting exports.foo)?
getExportedIdentifiers :: (MonadError ErrorMessage m)
                          => String
                          -> JSAST
                          -> m [String]
getExportedIdentifiers mname top
  | JSAstProgram stmts _ <- top = concat <$> traverse go stmts
  | otherwise = err InvalidTopLevel
  where
  err = throwError . ErrorInModule (ModuleIdentifier mname Foreign)

  go stmt
    | Just props <- matchExportsAssignment stmt
    = traverse toIdent (trailingCommaList props)
    | Just (True, name, _) <- matchMember stmt
    = pure [name]
    | otherwise
    = pure []

  toIdent (JSPropertyNameandValue name _ [_]) =
    extractLabel' name
  toIdent _ =
    err UnsupportedExport

  extractLabel' = maybe (err UnsupportedExport) pure . extractLabel

-- Matches JS statements like this:
-- var ModuleName = require("file");
matchRequire :: S.Set String
                -> ModuleIdentifier
                -> JSStatement
                -> Maybe (String, Either String ModuleIdentifier)
matchRequire mids mid stmt
  | JSVariable _ jsInit _ <- stmt
  , [JSVarInitExpression var varInit] <- commaList jsInit
  , JSIdentifier _ importName <- var
  , JSVarInit _ jsInitEx <- varInit
  , JSMemberExpression req _ argsE _ <- jsInitEx
  , JSIdentifier _ "require" <- req
  , [ Just importPath ] <- map fromStringLiteral (commaList argsE)
  , importPath' <- checkImportPath importPath mid mids
  = Just (importName, importPath')
  | otherwise
  = Nothing

-- Matches JS member declarations.
matchMember :: JSStatement -> Maybe (Bool, String, JSExpression)
matchMember stmt
  -- var foo = expr;
  | JSVariable _ jsInit _ <- stmt
  , [JSVarInitExpression var varInit] <- commaList jsInit
  , JSIdentifier _ name <- var
  , JSVarInit _ decl <- varInit
  = Just (False, name, decl)
  -- exports.foo = expr; exports["foo"] = expr;
  | JSAssignStatement e (JSAssign _) decl _ <- stmt
  , Just name <- accessor e
  = Just (True, name, decl)
  | otherwise
  = Nothing
  where
  accessor :: JSExpression -> Maybe String
  accessor (JSMemberDot exports _ nm)
    | JSIdentifier _ "exports" <- exports
    , JSIdentifier _ name <- nm
    = Just name
  accessor (JSMemberSquare exports _ nm _)
    | JSIdentifier _ "exports" <- exports
    , Just name <- fromStringLiteral nm
    = Just name
  accessor _ = Nothing

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

-- | Eliminate unused code based on the specified entry point set.
compile :: [Module] -> [ModuleIdentifier] -> [Module]
compile modules [] = modules
compile modules entryPoints = filteredModules
  where
  (graph, _, vertexFor) = graphFromEdges verts

  -- | The vertex set
  verts :: [(ModuleElement, Key, [Key])]
  verts = do
    Module mid _ els <- modules
    concatMap (toVertices mid) els
    where
    -- | Create a set of vertices for a module element.
    --
    -- Some special cases worth commenting on:
    --
    -- 1) Regular exports which simply export their own name do not count as dependencies.
    --    Regular exports which rename and reexport an operator do count, however.
    --
    -- 2) Require statements don't contribute towards dependencies, since they effectively get
    --    inlined wherever they are used inside other module elements.
    toVertices :: ModuleIdentifier -> ModuleElement -> [(ModuleElement, Key, [Key])]
    toVertices p m@(Member _ _ nm _ deps) = [(m, (p, nm), deps)]
    toVertices p m@(ExportsList exps) = mapMaybe toVertex exps
      where
      toVertex (ForeignReexport, nm, _, ks) = Just (m, (p, nm), ks)
      toVertex (RegularExport nm, nm1, _, ks) | nm /= nm1 = Just (m, (p, nm1), ks)
      toVertex _ = Nothing
    toVertices _ _ = []

  -- | The set of vertices whose connected components we are interested in keeping.
  entryPointVertices :: [Vertex]
  entryPointVertices = catMaybes $ do
    (_, k@(mid, _), _) <- verts
    guard $ mid `elem` entryPoints
    return (vertexFor k)

  -- | The set of vertices reachable from an entry point
  reachableSet :: S.Set Vertex
  reachableSet = S.fromList (concatMap (reachable graph) entryPointVertices)

  filteredModules :: [Module]
  filteredModules = map filterUsed modules
    where
    filterUsed :: Module -> Module
    filterUsed (Module mid fn ds) = Module mid fn (map filterExports (go ds))
      where
      go :: [ModuleElement] -> [ModuleElement]
      go [] = []
      go (d : rest)
        | not (isDeclUsed d) = skipDecl d : go rest
        | otherwise = d : go rest

      skipDecl :: ModuleElement -> ModuleElement
      skipDecl (Require s _ _) = Skip s
      skipDecl (Member s _ _ _ _) = Skip s
      skipDecl (ExportsList _) = Skip (JSEmptyStatement JSNoAnnot)
      skipDecl (Other s) = Skip s
      skipDecl (Skip s) = Skip s

      -- | Filter out the exports for members which aren't used.
      filterExports :: ModuleElement -> ModuleElement
      filterExports (ExportsList exps) = ExportsList (filter (\(_, nm, _, _) -> isKeyUsed (mid, nm)) exps)
      filterExports me = me

      isDeclUsed :: ModuleElement -> Bool
      isDeclUsed (Member _ _ nm _ _) = isKeyUsed (mid, nm)
      isDeclUsed _ = True

      isKeyUsed :: Key -> Bool
      isKeyUsed k
        | Just me <- vertexFor k = me `S.member` reachableSet
        | otherwise = False

-- | Topologically sort the module dependency graph, so that when we generate code, modules can be
-- defined in the right order.
sortModules :: [Module] -> [Module]
sortModules modules = map (\v -> case nodeFor v of (n, _, _) -> n) (reverse (topSort graph))
  where
  (graph, nodeFor, _) = graphFromEdges $ do
    m@(Module mid _ els) <- modules
    return (m, mid, mapMaybe getKey els)

  getKey :: ModuleElement -> Maybe ModuleIdentifier
  getKey (Require _ _ (Right mi)) = Just mi
  getKey _ = Nothing

-- | A module is empty if it contains no exported members (in other words,
-- if the only things left after dead code elimination are module imports and
-- "other" foreign code).
--
-- If a module is empty, we don't want to generate code for it.
isModuleEmpty :: Module -> Bool
isModuleEmpty (Module _ _ els) = all isElementEmpty els
  where
  isElementEmpty :: ModuleElement -> Bool
  isElementEmpty (ExportsList exps) = null exps
  isElementEmpty Require{} = True
  isElementEmpty (Other _) = True
  isElementEmpty (Skip _) = True
  isElementEmpty _ = False

-- | Generate code for a set of modules, including a call to main().
--
-- Modules get defined on the global PS object, as follows:
--
--     var PS = { };
--     (function(exports) {
--       ...
--     })(PS["Module.Name"] = PS["Module.Name"] || {});
--
-- In particular, a module and its foreign imports share the same namespace inside PS.
-- This saves us from having to generate unique names for a module and its foreign imports,
-- and is safe since a module shares a namespace with its foreign imports in PureScript as well
-- (so there is no way to have overlaps in code generated by psc).
codeGen :: Maybe String -- ^ main module
        -> String -- ^ namespace
        -> [Module] -- ^ input modules
        -> Maybe String -- ^ output filename
        -> (Maybe SourceMapping, String)
codeGen optionsMainModule optionsNamespace ms outFileOpt = (fmap sourceMapping outFileOpt, rendered)
  where
  rendered = renderToString (JSAstProgram (prelude : concatMap fst modulesJS ++ maybe [] runMain optionsMainModule) JSNoAnnot)

  sourceMapping :: String -> SourceMapping
  sourceMapping outFile = SourceMapping {
      smFile = outFile,
      smSourceRoot = Nothing,
      smMappings = concat $
        zipWith3 (\file (pos :: Int) positions ->
          map (\(porig, pgen) -> Mapping {
                mapOriginal = Just (Pos (fromIntegral $ porig + 1) 0)
              , mapSourceFile = pathToFile <$> file
              , mapGenerated = (Pos (fromIntegral $ pos + pgen) 0)
              , mapName = Nothing
              })
            (offsets (0,0) (Right 1 : positions)))
          moduleFns
          (scanl (+) (3 + moduleLength [prelude]) (map (3+) moduleLengths)) -- 3 lines between each module & at top
          (map snd modulesJS)
    }
    where
      pathToFile = makeRelative (takeDirectory outFile)

      offsets (m, n) (Left d:rest) = offsets (m+d, n) rest
      offsets (m, n) (Right d:rest) = map ((m+) &&& (n+)) [0 .. d - 1] ++ offsets (m+d, n+d) rest
      offsets _ _ = []

  moduleLength :: [JSStatement] -> Int
  moduleLength = everything (+) (mkQ 0 countw)
    where
      countw :: CommentAnnotation -> Int
      countw (WhiteSpace _ s) = length (filter (== '\n') s)
      countw _ = 0

  moduleLengths :: [Int]
  moduleLengths = map (sum . map (either (const 0) id) . snd) modulesJS
  moduleFns = map (\(Module _ fn _) -> fn) ms

  modulesJS = map moduleToJS ms

  moduleToJS :: Module -> ([JSStatement], [Either Int Int])
  moduleToJS (Module mn _ ds) = (wrap (moduleName mn) (indent (concat jsDecls)), lengths)
    where
    (jsDecls, lengths) = unzip $ map declToJS ds

    withLength :: [JSStatement] -> ([JSStatement], Either Int Int)
    withLength n = (n, Right $ moduleLength n)

    declToJS :: ModuleElement -> ([JSStatement], Either Int Int)
    declToJS (Member n _ _ _ _) = withLength [n]
    declToJS (Other n) = withLength [n]
    declToJS (Skip n) = ([], Left $ moduleLength [n])
    declToJS (Require _ nm req) = withLength
      [
        JSVariable lfsp
          (cList [
            JSVarInitExpression (JSIdentifier sp nm)
              (JSVarInit sp $ either require (moduleReference sp . moduleName) req )
          ]) (JSSemi JSNoAnnot)
      ]
    declToJS (ExportsList exps) = withLength $ map toExport exps

      where

      toExport :: (ExportType, String, JSExpression, [Key]) -> JSStatement
      toExport (_, nm, val, _) =
        JSAssignStatement
          (JSMemberSquare (JSIdentifier lfsp "exports") JSNoAnnot
            (str nm) JSNoAnnot)
          (JSAssign sp)
          val
          (JSSemi JSNoAnnot)

  -- comma lists are reverse-consed
  cList :: [a] -> JSCommaList a
  cList [] = JSLNil
  cList [x] = JSLOne x
  cList l = go $ reverse l
    where
      go [x] = JSLOne x
      go (h:t)= JSLCons (go t) JSNoAnnot h
      go [] = error "Invalid case in comma-list"

  indent :: [JSStatement] -> [JSStatement]
  indent = everywhere (mkT squash)
    where
    squash JSNoAnnot = JSAnnot (TokenPn 0 0 2) []
    squash (JSAnnot pos ann) = JSAnnot (keepCol pos) (map splat ann)
    squash JSAnnotSpace = JSAnnot (TokenPn 0 0 2) []

    splat (CommentA pos s) = CommentA (keepCol pos) s
    splat (WhiteSpace pos w) = WhiteSpace (keepCol pos) w
    splat ann = ann

    keepCol (TokenPn _ _ c) = TokenPn 0 0 (if c >= 0 then c + 2 else 2)

  prelude :: JSStatement
  prelude = JSVariable (JSAnnot tokenPosnEmpty [ CommentA tokenPosnEmpty $ "// Generated by psc-bundle " ++ showVersion Paths.version
                                               , WhiteSpace tokenPosnEmpty "\n" ])
              (cList [
                JSVarInitExpression (JSIdentifier sp optionsNamespace)
                  (JSVarInit sp (emptyObj sp))
              ]) (JSSemi JSNoAnnot)

  require :: String -> JSExpression
  require mn =
    JSMemberExpression (JSIdentifier JSNoAnnot "require") JSNoAnnot (cList [ str mn ]) JSNoAnnot

  moduleReference :: JSAnnot -> String -> JSExpression
  moduleReference a mn =
    JSMemberSquare (JSIdentifier a optionsNamespace) JSNoAnnot
      (str mn) JSNoAnnot

  str :: String -> JSExpression
  str s = JSStringLiteral JSNoAnnot $ "\"" ++ s ++ "\""


  emptyObj :: JSAnnot -> JSExpression
  emptyObj a = JSObjectLiteral a (JSCTLNone JSLNil) JSNoAnnot

  wrap :: String -> [JSStatement] -> [JSStatement]
  wrap mn ds =
    [
    JSMethodCall (JSExpressionParen lf (JSFunctionExpression JSNoAnnot JSIdentNone JSNoAnnot
                                                (JSLOne (JSIdentName JSNoAnnot "exports")) JSNoAnnot
                                                (JSBlock sp (lfHead ds) lf)) -- \n not quite in right place
                                    JSNoAnnot)
                  JSNoAnnot
                  (JSLOne (JSAssignExpression (moduleReference JSNoAnnot mn) (JSAssign sp)
                            (JSExpressionBinary (moduleReference sp mn) (JSBinOpOr sp) (emptyObj sp))))
                  JSNoAnnot
                  (JSSemi JSNoAnnot)
    ]
    where
      lfHead (h:t) = addAnn (WhiteSpace tokenPosnEmpty "\n  ") h : t
      lfHead x = x

      addAnn :: CommentAnnotation -> JSStatement -> JSStatement
      addAnn a (JSExpressionStatement (JSStringLiteral ann s) _) =
        JSExpressionStatement (JSStringLiteral (appendAnn a ann) s) (JSSemi JSNoAnnot)
      addAnn _ x = x

      appendAnn a JSNoAnnot = JSAnnot tokenPosnEmpty [a]
      appendAnn a (JSAnnot _ anns) = JSAnnot tokenPosnEmpty (a:anns ++ [WhiteSpace tokenPosnEmpty "  "])
      appendAnn a JSAnnotSpace = JSAnnot tokenPosnEmpty [a]

  runMain :: String -> [JSStatement]
  runMain mn =
    [JSMethodCall
      (JSMemberDot (moduleReference lf mn) JSNoAnnot
        (JSIdentifier JSNoAnnot "main"))
      JSNoAnnot (cList []) JSNoAnnot (JSSemi JSNoAnnot)]

  lf :: JSAnnot
  lf = JSAnnot tokenPosnEmpty [ WhiteSpace tokenPosnEmpty "\n" ]


  lfsp :: JSAnnot
  lfsp = JSAnnot tokenPosnEmpty [ WhiteSpace tokenPosnEmpty "\n  " ]

  sp :: JSAnnot
  sp = JSAnnot tokenPosnEmpty [ WhiteSpace tokenPosnEmpty " " ]

-- | The bundling function.
-- This function performs dead code elimination, filters empty modules
-- and generates and prints the final JavaScript bundle.
bundleSM :: (MonadError ErrorMessage m)
       => [(ModuleIdentifier, Maybe FilePath, String)] -- ^ The input modules.  Each module should be javascript rendered from 'Language.PureScript.Make' or @psc@.
       -> [ModuleIdentifier] -- ^ Entry points.  These module identifiers are used as the roots for dead-code elimination
       -> Maybe String -- ^ An optional main module.
       -> String -- ^ The namespace (e.g. PS).
       -> Maybe FilePath -- ^ The output file name (if there is one - in which case generate source map)
       -> Bool
       -> m (Maybe SourceMapping, String)
bundleSM inputStrs entryPoints mainModule namespace outFilename shouldUncurry = do
  let mid (a,_,_) = a
  forM_ mainModule $ \mname ->
    when (mname `notElem` map (moduleName . mid) inputStrs) (throwError (MissingMainModule mname))
  forM_ entryPoints $ \mIdent ->
    when (mIdent `notElem` map mid inputStrs) (throwError (MissingEntryPoint (moduleName mIdent)))
  input <- forM inputStrs $ \(ident, filename, js) -> do
                ast <- either (throwError . ErrorInModule ident . UnableToParseModule) pure $ parse js (moduleName ident)
                return (ident, filename, ast)

  let mids = S.fromList (map (moduleName . mid) input)

  modules <- traverse (fmap withDeps . (\(a,fn,c) -> toModule mids a fn c)) input

  let compiled = compile modules entryPoints

  -- The uncurry optimization performs dead code elemination (DCE) once and then
  -- generates uncurried variants of the surviving functions. We need to
  -- perform DCE again to throw away variants (curried or uncurried) that
  -- aren't called after choosing the appropriate variant for each call site.
  -- This two-step process avoids generating variants for functions that
  -- are dead weight anyway.
  compiled' <- if shouldUncurry
                    then do
                        let modules' = uncurryFunc compiled entryPoints
                        modules'' <- traverse (fmap withDeps . pure) modules'   -- traverse and compile again
                        return (compile modules'' entryPoints)
                    else return compiled

  let sorted   = sortModules (filter (not . isModuleEmpty) compiled')

  return (codeGen mainModule namespace sorted outFilename)

-- | The bundling function.
-- This function performs dead code elimination, filters empty modules
-- and generates and prints the final JavaScript bundle.
bundle :: (MonadError ErrorMessage m)
       => [(ModuleIdentifier, String)] -- ^ The input modules.  Each module should be javascript rendered from 'Language.PureScript.Make' or @psc@.
       -> [ModuleIdentifier] -- ^ Entry points.  These module identifiers are used as the roots for dead-code elimination
       -> Maybe String -- ^ An optional main module.
       -> String -- ^ The namespace (e.g. PS).
       -> Bool
       -> m String
bundle inputStrs entryPoints mainModule namespace shouldUncurry = snd <$> bundleSM (map (\(a,b) -> (a,Nothing,b)) inputStrs) entryPoints mainModule namespace Nothing shouldUncurry
