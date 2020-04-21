module Language.PureScript.Interactive.Module where

import           Prelude.Compat

import qualified Language.PureScript as P
import qualified Language.PureScript.CST as CST
import           Language.PureScript.Interactive.Types
import           System.Directory (getCurrentDirectory)
import           System.FilePath (pathSeparator, makeRelative)
import           System.IO.UTF8 (readUTF8FileT, readUTF8FilesT)

-- * Support Module

-- | The name of the PSCI support module
supportModuleName :: P.ModuleName
supportModuleName = fst initialInteractivePrint

-- | Checks if the Console module is defined
supportModuleIsDefined :: [P.ModuleName] -> Bool
supportModuleIsDefined = any ((== supportModuleName))

-- * Module Management

-- | Loads a file for use with imports.
loadModule :: FilePath -> IO (Either String [P.Module])
loadModule filename = do
  pwd <- getCurrentDirectory
  content <- readUTF8FileT filename
  return $
    either (Left . P.prettyPrintMultipleErrors P.defaultPPEOptions {P.ppeRelativeDirectory = pwd}) (Right . map snd) $
      CST.parseFromFiles id [(filename, content)]

-- | Load all modules.
loadAllModules :: [FilePath] -> IO (Either P.MultipleErrors [(FilePath, P.Module)])
loadAllModules files = do
  pwd <- getCurrentDirectory
  filesAndContent <- readUTF8FilesT files
  return $ CST.parseFromFiles (makeRelative pwd) filesAndContent

-- |
-- Makes a volatile module to execute the current expression.
--
createTemporaryModule :: Bool -> PSCiState -> P.Expr -> P.Module
createTemporaryModule exec st val =
  let
    imports       = psciImportedModules st
    lets          = psciLetBindings st
    moduleName    = P.ModuleName "$PSCI"
    effModuleName = P.ModuleName "Effect"
    effImport     = (effModuleName, P.Implicit, Just (P.ModuleName "$Effect"))
    supportImport = (fst (psciInteractivePrint st), P.Implicit, Just (P.ModuleName "$Support"))
    eval          = P.Var internalSpan (P.Qualified (Just (P.ModuleName "$Support")) (snd (psciInteractivePrint st)))
    mainValue     = P.App eval (P.Var internalSpan (P.Qualified Nothing (P.Ident "it")))
    itDecl        = P.ValueDecl (internalSpan, []) (P.Ident "it") P.Public [] [P.MkUnguarded val]
    typeDecl      = P.TypeDeclaration
                      (P.TypeDeclarationData (internalSpan, []) (P.Ident "$main")
                        (P.srcTypeApp
                          (P.srcTypeConstructor
                            (P.Qualified (Just (P.ModuleName "$Effect")) (P.ProperName "Effect")))
                                  P.srcTypeWildcard))
    mainDecl      = P.ValueDecl (internalSpan, []) (P.Ident "$main") P.Public [] [P.MkUnguarded mainValue]
    decls         = if exec then [itDecl, typeDecl, mainDecl] else [itDecl]
  in
    P.Module internalSpan
             [] moduleName
             ((importDecl `map` (effImport : supportImport : imports)) ++ lets ++ decls)
             Nothing


-- |
-- Makes a volatile module to hold a non-qualified type synonym for a fully-qualified data type declaration.
--
createTemporaryModuleForKind :: PSCiState -> P.SourceType -> P.Module
createTemporaryModuleForKind st typ =
  let
    imports    = psciImportedModules st
    lets       = psciLetBindings st
    moduleName = P.ModuleName "$PSCI"
    itDecl     = P.TypeSynonymDeclaration (internalSpan, []) (P.ProperName "IT") [] typ
  in
    P.Module internalSpan [] moduleName ((importDecl `map` imports) ++ lets ++ [itDecl]) Nothing

-- |
-- Makes a volatile module to execute the current imports.
--
createTemporaryModuleForImports :: PSCiState -> P.Module
createTemporaryModuleForImports st =
  let
    imports    = psciImportedModules st
    moduleName = P.ModuleName "$PSCI"
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
