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
-- | Bundles compiled PureScript modules for the browser.
--
-- This module takes as input the individual generated modules from 'Language.PureScript.Make' and
-- performs dead code elimination, filters empty modules,
-- and generates the final Javascript bundle.
-----------------------------------------------------------------------------

{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Language.PureScript.Bundle (
     bundle
   , ModuleIdentifier(..)
   , moduleName
   , ModuleType(..)
   , ErrorMessage(..)
   , printErrorMessage
) where

import Prelude ()
import Prelude.Compat

import Data.List (nub, stripPrefix)
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import Data.Generics (everything, everywhere, mkQ, mkT)
import Data.Graph
import Data.Version (showVersion)
import Language.PureScript.BundleOpt

import qualified Data.Set as S

import Control.Monad
import Control.Monad.Error.Class
import Language.JavaScript.Parser
import Language.PureScript.BundleTypes

import qualified Paths_purescript as Paths

-- | The type of error messages. We separate generation and rendering of errors using a data
-- type, in case we need to match on error types later.
data ErrorMessage
  = UnsupportedModulePath String
  | InvalidTopLevel
  | UnableToParseModule String
  | UnsupportedExport
  | ErrorInModule ModuleIdentifier ErrorMessage
  deriving (Show, Read)

-- | Prepare an error message for consumption by humans.
printErrorMessage :: ErrorMessage -> [String]
printErrorMessage (UnsupportedModulePath s) =
  [ "A CommonJS module has an unsupported name (" ++ show s ++ ")."
  , "The following file names are supported:"
  , "  1) index.js (psc native modules)"
  , "  2) foreign.js (psc foreign modules)"
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

-- | Calculate the ModuleIdentifier which a require(...) statement imports.
checkImportPath :: Maybe FilePath -> String -> ModuleIdentifier -> S.Set String -> Either String ModuleIdentifier
checkImportPath _ "./foreign" m _ =
  Right (ModuleIdentifier (moduleName m) Foreign)
checkImportPath requirePath name _ names
  | Just name' <- stripPrefix (fromMaybe "" requirePath) name
  , name' `S.member` names = Right (ModuleIdentifier name' Regular)
checkImportPath _ name _ _ = Left name

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
withDeps (Module modulePath es) = Module modulePath (map expandDeps es)
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
  expandDeps (Member n f nm decl _) = Member n f nm decl (nub (concatMap (dependencies modulePath) decl))
  expandDeps (ExportsList exps) = ExportsList (map expand exps)
      where
      expand (ty, nm, n1, _) = (ty, nm, n1, nub (dependencies modulePath n1))
  expandDeps other = other

  dependencies :: ModuleIdentifier -> JSNode -> [(ModuleIdentifier, String)]
  dependencies m = everything (++) (mkQ [] toReference)
    where
    toReference :: Node -> [(ModuleIdentifier, String)]
    toReference (JSMemberDot [ mn ] _ nm)
      | JSIdentifier mn' <- node mn
      , JSIdentifier nm' <- node nm
      , Just mid <- lookup mn' imports
      = [(mid, nm')]
    toReference (JSMemberSquare [ mn ] _ nm _)
      | JSIdentifier mn' <- node mn
      , JSExpression [ s ] <- node nm
      , JSStringLiteral _ nm' <- node s
      , Just mid <- lookup mn' imports
      = [(mid, nm')]
    toReference (JSIdentifier nm)
      | nm `elem` boundNames
      = [(m, nm)]
    toReference _ = []

-- | Attempt to create a Module from a Javascript AST.
--
-- Each type of module element is matched using pattern guards, and everything else is bundled into the
-- Other constructor.
toModule :: forall m. (Applicative m, MonadError ErrorMessage m) => Maybe FilePath -> S.Set String -> ModuleIdentifier -> JSNode -> m Module
toModule requirePath mids mid top
  | JSSourceElementsTop ns <- node top = Module mid <$> traverse toModuleElement ns
  | otherwise = err InvalidTopLevel
  where
  err = throwError . ErrorInModule mid

  toModuleElement :: JSNode -> m ModuleElement
  toModuleElement n
    | JSVariables var [ varIntro ] _ <- node n
    , JSLiteral "var" <- node var
    , JSVarDecl impN [ eq, req, impP ] <- node varIntro
    , JSIdentifier importName <- node impN
    , JSLiteral "=" <- node eq
    , JSIdentifier "require" <- node req
    , JSArguments _ [ impS ] _ <- node impP
    , JSStringLiteral _ importPath <- node impS
    , importPath' <- checkImportPath requirePath importPath mid mids
    = pure (Require n importName importPath')
  toModuleElement n
    | JSVariables var [ varIntro ] _ <- node n
    , JSLiteral "var" <- node var
    , JSVarDecl declN (eq : decl) <- node varIntro
    , JSIdentifier name <- node declN
    , JSLiteral "=" <- node eq
    = pure (Member n False name decl [])
  toModuleElement n
    | JSExpression (e : op : decl) <- node n
    , Just name <- accessor (node e)
    , JSOperator eq <- node op
    , JSLiteral "=" <- node eq
    = pure (Member n True name decl [])
  toModuleElement n
    | JSExpression (mnExp : op : obj: _) <- node n
    , JSMemberDot [ mn ] _ e <- node mnExp
    , JSIdentifier "module" <- node mn
    , JSIdentifier "exports" <- node e
    , JSOperator eq <- node op
    , JSLiteral "=" <- node eq
    , JSObjectLiteral _ props _ <- node obj
    = ExportsList <$> traverse toExport (filter (not . isSeparator) (map node props))
    where
    toExport :: Node -> m (ExportType, String, JSNode, [Key])
    toExport (JSPropertyNameandValue name _ [val] ) =
      (,,val,[]) <$> exportType (node val)
                 <*> extractLabel (node name)
    toExport _ = err UnsupportedExport

    exportType :: Node -> m ExportType
    exportType (JSMemberDot [f] _ _)
      | JSIdentifier "$foreign" <- node f
      = pure ForeignReexport
    exportType (JSMemberSquare [f] _ _ _)
      | JSIdentifier "$foreign" <- node f
      = pure ForeignReexport
    exportType (JSIdentifier s) = pure (RegularExport s)
    exportType _ = err UnsupportedExport

    extractLabel :: Node -> m String
    extractLabel (JSStringLiteral _ nm) = pure nm
    extractLabel (JSIdentifier nm) = pure nm
    extractLabel _ = err UnsupportedExport

    isSeparator :: Node -> Bool
    isSeparator (JSLiteral ",") = True
    isSeparator _ = False
  toModuleElement other = pure (Other other)

-- | Eliminate unused code based on the specified entry point set.
compile :: [Module] -> [ModuleIdentifier] -> [Module]
compile modules [] = modules
compile modules entryPoints = filteredModules
  where
  (graph, _, vertexFor) = graphFromEdges verts

  -- | The vertex set
  verts :: [(ModuleElement, Key, [Key])]
  verts = do
    Module mid els <- modules
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
    filterUsed (Module mid ds) = Module mid (map filterExports (go ds))
      where
      go :: [ModuleElement] -> [ModuleElement]
      go [] = []
      go (d : Other semi : rest)
        | JSLiteral ";" <- node semi
        , not (isDeclUsed d)
        = go rest
      go (d : rest)
        | not (isDeclUsed d) = go rest
        | otherwise = d : go rest

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
    m@(Module mid els) <- modules
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
isModuleEmpty (Module _ els) = all isElementEmpty els
  where
  isElementEmpty :: ModuleElement -> Bool
  isElementEmpty (ExportsList exps) = null exps
  isElementEmpty Require{} = True
  isElementEmpty (Other _) = True
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
        -> String
codeGen optionsMainModule optionsNamespace ms = renderToString (NN (JSSourceElementsTop (prelude ++ concatMap moduleToJS ms ++ maybe [] runMain optionsMainModule)))
  where
  moduleToJS :: Module -> [JSNode]
  moduleToJS (Module mn ds) = wrap (moduleName mn) (indent (concatMap declToJS ds))
    where
    declToJS :: ModuleElement -> [JSNode]
    declToJS (Member n _ _ _ _) = [n]
    declToJS (Other n) = [n]
    declToJS (Require _ nm req) =
      [ NN (JSVariables (NT (JSLiteral "var") tokenPosnEmpty [ WhiteSpace tokenPosnEmpty "\n  " ])
                        [ NN (JSVarDecl (sp (JSIdentifier nm))
                                        (sp (JSLiteral "=") : either require (return . moduleReference sp . moduleName) req))
                        ]
                        (nt (JSLiteral ";"))) ]
    declToJS (ExportsList exps) = map toExport exps

      where
      toExport :: (ExportType, String, JSNode, [Key]) -> JSNode
      toExport (_, nm, val, _) =
        NN (JSExpression [ NN (JSMemberSquare [ NT (JSIdentifier "exports") tokenPosnEmpty [ WhiteSpace tokenPosnEmpty "\n  " ] ]
                                              (nt (JSLiteral "["))
                                              (NN (JSExpression [ nt (JSStringLiteral '"' nm) ]))
                                              (nt (JSLiteral "]")))
                         , NN (JSOperator (sp (JSLiteral "=")))
                         , reindent val
                         , nt (JSLiteral ";")
                         ])

      reindent :: JSNode -> JSNode
      reindent (NT n _ _) = sp n
      reindent nn = nn

  indent :: [JSNode] -> [JSNode]
  indent = everywhere (mkT squash)
    where
    squash (NT n pos ann) = NT n (keepCol pos) (map splat ann)
    squash nn = nn

    splat (CommentA pos s) = CommentA (keepCol pos) s
    splat (WhiteSpace pos w) = WhiteSpace (keepCol pos) w
    splat ann = ann

    keepCol (TokenPn _ _ c) = TokenPn 0 0 (c + 2)

  prelude :: [JSNode]
  prelude =
    [ NN (JSVariables (NT (JSLiteral "var") tokenPosnEmpty [ CommentA tokenPosnEmpty ("// Generated by psc-bundle " ++ showVersion Paths.version)
                                                           , WhiteSpace tokenPosnEmpty "\n"
                                                           ])
                                                           [ NN (JSVarDecl (sp (JSIdentifier optionsNamespace))
                                                                           [ sp (JSLiteral "=")
                                                                           , NN (JSObjectLiteral (sp (JSLiteral "{"))
                                                                                                 []
                                                                                                 (sp (JSLiteral "}")))
                                                                           ])
                                                           ]
                                                           (nt (JSLiteral ";")))
    , lf
    ]

  require :: String -> [JSNode]
  require mn = [ sp (JSIdentifier "require")
               , NN (JSArguments (nt (JSLiteral "(")) [ nt (JSStringLiteral '"' mn) ] (nt (JSLiteral ")")))
               ]

  moduleReference :: (Node -> JSNode) -> String -> JSNode
  moduleReference f mn =
    NN (JSMemberSquare [ f (JSIdentifier optionsNamespace) ]
                       (nt (JSLiteral "["))
                       (NN (JSExpression [ nt (JSStringLiteral '"' mn) ]))
                       (nt (JSLiteral "]")))

  wrap :: String -> [JSNode] -> [JSNode]
  wrap mn ds =
    [ NN (JSExpression [ NN (JSExpressionParen (nt (JSLiteral "("))
                                               (NN (JSExpression [ NN (JSFunctionExpression (nt (JSLiteral "function"))
                                                                                            []
                                                                                            (nt (JSLiteral "(") ) [nt (JSIdentifier "exports")] (nt (JSLiteral ")"))
                                                                                            (NN (JSBlock [sp (JSLiteral "{")]
                                                                                                         (lf : ds)
                                                                                                         [nl (JSLiteral "}")])))]))
                                               (nt (JSLiteral ")")))
                       , NN (JSArguments (nt (JSLiteral "("))
                                         [ NN (JSExpression [ moduleReference nt mn
                                                            , NN (JSOperator (sp (JSLiteral "=")))
                                                            , NN (JSExpressionBinary "||"
                                                                                     [ moduleReference sp mn ]
                                                                                     (sp (JSLiteral "||"))
                                                                                     [ emptyObj ])
                                                            ])
                                         ]
                                         (nt (JSLiteral ")")))
                       ])
    , nt (JSLiteral ";")
    , lf
    ]
    where
    emptyObj = NN (JSObjectLiteral (sp (JSLiteral "{")) [] (nt (JSLiteral "}")))

  runMain :: String -> [JSNode]
  runMain mn =
    [ NN (JSExpression [ NN (JSMemberDot [ NN (JSMemberSquare [ nl (JSIdentifier optionsNamespace) ]
                                                              (nt (JSLiteral "["))
                                                              (NN (JSExpression [ nt (JSStringLiteral '"' mn) ]))
                                                              (nt (JSLiteral "]")))
                                         ]
                                         (nt (JSLiteral "."))
                                         (nt (JSIdentifier "main")))
                       , NN (JSArguments (nt (JSLiteral "(")) [] (nt (JSLiteral ")")))
                       ])
    , nt (JSLiteral ";")
    ]


-- | The bundling function.
-- This function performs dead code elimination, filters empty modules
-- and generates and prints the final Javascript bundle.
bundle :: (Applicative m, MonadError ErrorMessage m)
       => [(ModuleIdentifier, String)] -- ^ The input modules.  Each module should be javascript rendered from 'Language.PureScript.Make' or @psc@.
       -> [ModuleIdentifier] -- ^ Entry points.  These module identifiers are used as the roots for dead-code elimination
       -> Maybe String -- ^ An optional main module.
       -> String -- ^ The namespace (e.g. PS).
       -> Maybe FilePath -- ^ The require path prefix
       -> Maybe String
       -> m String
bundle inputStrs entryPoints mainModule namespace requirePath optimize = do
  let shouldUncurry = case optimize of
                        Nothing -> False
                        Just "uncurry" -> True
                        Just "u" -> True
                        Just "all" -> True
                        Just "a" -> True
                        Just _ -> False
      secondRun = True
  input <- forM inputStrs $ \(ident, js) -> do
                ast <- either (throwError . ErrorInModule ident . UnableToParseModule) pure $ parse js (moduleName ident)
                return (ident, ast)

  let mids = S.fromList (map (moduleName . fst) input)

  modules <- traverse (fmap withDeps . uncurry (toModule requirePath mids)) input

  let compiled = compile modules entryPoints

  compiled' <- if shouldUncurry
                    then do
                        let modules' = uncurryFunc compiled entryPoints
                        if secondRun
                            then do
                                modules'' <- traverse (fmap withDeps . pure) modules' -- traverse and compile again
                                return (compile modules'' entryPoints)
                            else return modules'
                    else return compiled

  let sorted   = sortModules (filter (not . isModuleEmpty) compiled')

  return (codeGen mainModule namespace sorted)
