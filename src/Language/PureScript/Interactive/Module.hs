module Language.PureScript.Interactive.Module where

import           Prelude.Compat

import           Control.Monad
import qualified Language.PureScript as P
import           Language.PureScript.Interactive.Types
import           System.Directory (getCurrentDirectory)
import           System.FilePath (pathSeparator, makeRelative)
import           System.IO.UTF8 (readUTF8FileT)

-- * Support Module

-- | The name of the PSCI support module
supportModuleName :: P.ModuleName
supportModuleName = P.moduleNameFromString "PSCI.Support"

-- | Checks if the Console module is defined
supportModuleIsDefined :: [P.Module] -> Bool
supportModuleIsDefined = any ((== supportModuleName) . P.getModuleName)

-- * Module Management

-- | Loads a file for use with imports.
loadModule :: FilePath -> IO (Either String [P.Module])
loadModule filename = do
  pwd <- getCurrentDirectory
  content <- readUTF8FileT filename
  return $
    either (Left . P.prettyPrintMultipleErrors P.defaultPPEOptions {P.ppeRelativeDirectory = pwd}) (Right . map snd) $
      P.parseModulesFromFiles id [(filename, content)]

-- | Load all modules.
loadAllModules :: [FilePath] -> IO (Either P.MultipleErrors [(FilePath, P.Module)])
loadAllModules files = do
  pwd <- getCurrentDirectory
  filesAndContent <- forM files $ \filename -> do
    content <- readUTF8FileT filename
    return (filename, content)
  return $ P.parseModulesFromFiles (makeRelative pwd) filesAndContent

-- |
-- Makes a volatile module to execute the current expression.
--
createTemporaryModule :: Bool -> PSCiState -> P.Expr -> P.Module
createTemporaryModule exec PSCiState{psciImportedModules = imports, psciLetBindings = lets} val =
  let
    moduleName    = P.ModuleName [P.ProperName "$PSCI"]
    effModuleName = P.moduleNameFromString "Control.Monad.Eff"
    effImport     = (effModuleName, P.Implicit, Just (P.ModuleName [P.ProperName "$Eff"]))
    supportImport = (supportModuleName, P.Implicit, Just (P.ModuleName [P.ProperName "$Support"]))
    eval          = P.Var (P.Qualified (Just (P.ModuleName [P.ProperName "$Support"])) (P.Ident "eval"))
    mainValue     = P.App eval (P.Var (P.Qualified Nothing (P.Ident "it")))
    itDecl        = P.ValueDeclaration (P.Ident "it") P.Public [] [P.MkUnguarded val]
    typeDecl      = P.TypeDeclaration (P.Ident "$main")
                      (P.TypeApp
                        (P.TypeApp
                          (P.TypeConstructor
                            (P.Qualified (Just (P.ModuleName [P.ProperName "$Eff"])) (P.ProperName "Eff")))
                              (P.TypeWildcard internalSpan))
                                (P.TypeWildcard internalSpan))
    mainDecl      = P.ValueDeclaration (P.Ident "$main") P.Public [] [P.MkUnguarded mainValue]
    decls         = if exec then [itDecl, typeDecl, mainDecl] else [itDecl]
  in
    P.Module internalSpan
             [] moduleName
             ((importDecl `map` (effImport : supportImport : imports)) ++ lets ++ decls)
             Nothing


-- |
-- Makes a volatile module to hold a non-qualified type synonym for a fully-qualified data type declaration.
--
createTemporaryModuleForKind :: PSCiState -> P.Type -> P.Module
createTemporaryModuleForKind PSCiState{psciImportedModules = imports, psciLetBindings = lets} typ =
  let
    moduleName = P.ModuleName [P.ProperName "$PSCI"]
    itDecl = P.TypeSynonymDeclaration (P.ProperName "IT") [] typ
  in
    P.Module internalSpan [] moduleName ((importDecl `map` imports) ++ lets ++ [itDecl]) Nothing

-- |
-- Makes a volatile module to execute the current imports.
--
createTemporaryModuleForImports :: PSCiState -> P.Module
createTemporaryModuleForImports PSCiState{psciImportedModules = imports} =
  let
    moduleName = P.ModuleName [P.ProperName "$PSCI"]
  in
    P.Module internalSpan [] moduleName (importDecl `map` imports) Nothing

importDecl :: ImportedModule -> P.Declaration
importDecl (mn, declType, asQ) = P.ImportDeclaration (internalSpan, []) mn declType asQ

indexFile :: FilePath
indexFile = ".psci_modules" ++ pathSeparator : "index.js"

modulesDir :: FilePath
modulesDir = ".psci_modules" ++ pathSeparator : "node_modules"

internalSpan :: P.SourceSpan
internalSpan = P.internalModuleSourceSpan "<internal>"
