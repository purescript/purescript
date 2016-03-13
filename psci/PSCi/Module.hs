module PSCi.Module where

import Prelude ()
import Prelude.Compat

import qualified Language.PureScript as P
import PSCi.Types
import System.FilePath (pathSeparator)
import Control.Monad

-- | The name of the PSCI support module
supportModuleName :: P.ModuleName
supportModuleName = P.ModuleName [P.ProperName "$PSCI", P.ProperName "Support"]

-- | Support module, contains code to evaluate terms
supportModule :: P.Module
supportModule =
  case P.parseModulesFromFiles id [("", code)] of
    Right [(_, P.Module ss cs _ ds exps)] -> P.Module ss cs supportModuleName ds exps
    _ -> P.internalError "Support module could not be parsed"
  where
  code :: String
  code = unlines
    [ "module S where"
    , ""
    , "import Prelude"
    , "import Control.Monad.Eff"
    , "import Control.Monad.Eff.Console"
    , "import Control.Monad.Eff.Unsafe"
    , ""
    , "class Eval a where"
    , "  eval :: a -> Eff (console :: CONSOLE) Unit"
    , ""
    , "instance evalShow :: (Show a) => Eval a where"
    , "  eval = print"
    , ""
    , "instance evalEff :: (Eval a) => Eval (Eff eff a) where"
    , "  eval x = unsafeInterleaveEff x >>= eval"
    ]

-- Module Management

-- |
-- Loads a file for use with imports.
--
loadModule :: FilePath -> IO (Either String [P.Module])
loadModule filename = do
  content <- readFile filename
  return $ either (Left . P.prettyPrintMultipleErrors False) (Right . map snd) $ P.parseModulesFromFiles id [(filename, content)]

-- |
-- Load all modules.
--
loadAllModules :: [FilePath] -> IO (Either P.MultipleErrors [(FilePath, P.Module)])
loadAllModules files = do
  filesAndContent <- forM files $ \filename -> do
    content <- readFile filename
    return (filename, content)
  return $ P.parseModulesFromFiles id filesAndContent


-- |
-- Makes a volatile module to execute the current expression.
--
createTemporaryModule :: Bool -> PSCiState -> P.Expr -> P.Module
createTemporaryModule exec PSCiState{psciImportedModules = imports, psciLetBindings = lets} val =
  let
    moduleName = P.ModuleName [P.ProperName "$PSCI"]
    trace = P.Var (P.Qualified (Just supportModuleName) (P.Ident "eval"))
    mainValue = P.App trace (P.Var (P.Qualified Nothing (P.Ident "it")))
    itDecl = P.ValueDeclaration (P.Ident "it") P.Public [] $ Right val
    mainDecl = P.ValueDeclaration (P.Ident "$main") P.Public [] $ Right mainValue
    decls = if exec then [itDecl, mainDecl] else [itDecl]
  in
    P.Module (P.internalModuleSourceSpan "<internal>") [] moduleName ((importDecl `map` imports) ++ lets ++ decls) Nothing


-- |
-- Makes a volatile module to hold a non-qualified type synonym for a fully-qualified data type declaration.
--
createTemporaryModuleForKind :: PSCiState -> P.Type -> P.Module
createTemporaryModuleForKind PSCiState{psciImportedModules = imports, psciLetBindings = lets} typ =
  let
    moduleName = P.ModuleName [P.ProperName "$PSCI"]
    itDecl = P.TypeSynonymDeclaration (P.ProperName "IT") [] typ
  in
    P.Module (P.internalModuleSourceSpan "<internal>") [] moduleName ((importDecl `map` imports) ++ lets ++ [itDecl]) Nothing

-- |
-- Makes a volatile module to execute the current imports.
--
createTemporaryModuleForImports :: PSCiState -> P.Module
createTemporaryModuleForImports PSCiState{psciImportedModules = imports} =
  let
    moduleName = P.ModuleName [P.ProperName "$PSCI"]
  in
    P.Module (P.internalModuleSourceSpan "<internal>") [] moduleName (importDecl `map` imports) Nothing

importDecl :: ImportedModule -> P.Declaration
importDecl (mn, declType, asQ) = P.ImportDeclaration mn declType asQ False

indexFile :: FilePath
indexFile = ".psci_modules" ++ pathSeparator : "index.js"

modulesDir :: FilePath
modulesDir = ".psci_modules" ++ pathSeparator : "node_modules"
