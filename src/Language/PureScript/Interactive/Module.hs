module Language.PureScript.Interactive.Module where

import           Prelude.Compat

import           Control.Monad
import qualified Language.PureScript as P
import           Language.PureScript.Interactive.Types
import           System.FilePath (pathSeparator)
import           System.IO.UTF8 (readUTF8File)

-- * Support Module

-- | The name of the PSCI support module
supportModuleName :: P.ModuleName
supportModuleName = P.moduleNameFromString "PSCI.Support"

-- | Checks if the Console module is defined
supportModuleIsDefined :: [P.Module] -> Bool
supportModuleIsDefined = any ((== supportModuleName) . P.getModuleName)

-- * Module Management

-- |
-- Loads a file for use with imports.
--
loadModule :: FilePath -> IO (Either String [P.Module])
loadModule filename = do
  content <- readUTF8File filename
  return $ either (Left . P.prettyPrintMultipleErrors False) (Right . map snd) $ P.parseModulesFromFiles id [(filename, content)]

-- |
-- Load all modules.
--
loadAllModules :: [FilePath] -> IO (Either P.MultipleErrors [(FilePath, P.Module)])
loadAllModules files = do
  filesAndContent <- forM files $ \filename -> do
    content <- readUTF8File filename
    return (filename, content)
  return $ P.parseModulesFromFiles id filesAndContent

-- |
-- Makes a volatile module to execute the current expression.
--
createTemporaryModule :: Bool -> PSCiState -> P.Expr -> P.Module
createTemporaryModule exec PSCiState{psciImportedModules = imports, psciLetBindings = lets} val =
  let
    moduleName    = P.ModuleName [P.ProperName "$PSCI"]
    supportImport = (supportModuleName, P.Implicit, Just (P.ModuleName [P.ProperName "$Support"]))
    eval          = P.Var (P.Qualified (Just (P.ModuleName [P.ProperName "$Support"])) (P.Ident "eval"))
    mainValue     = P.App eval (P.Var (P.Qualified Nothing (P.Ident "it")))
    itDecl        = P.ValueDeclaration (P.Ident "it") P.Public [] $ Right val
    mainDecl      = P.ValueDeclaration (P.Ident "$main") P.Public [] $ Right mainValue
    decls         = if exec then [itDecl, mainDecl] else [itDecl]
  in
    P.Module (P.internalModuleSourceSpan "<internal>") [] moduleName ((importDecl `map` (supportImport : imports)) ++ lets ++ decls) Nothing


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
importDecl (mn, declType, asQ) = P.ImportDeclaration mn declType asQ

indexFile :: FilePath
indexFile = ".psci_modules" ++ pathSeparator : "index.js"

modulesDir :: FilePath
modulesDir = ".psci_modules" ++ pathSeparator : "node_modules"
