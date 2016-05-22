-- |
-- Type declarations and associated basic functions for PSCI.
--
module Language.PureScript.Interactive.Types where

import Prelude.Compat

import qualified Language.PureScript as P

-- | The PSCI configuration.
--
-- These configuration values do not change during execution.
--
data PSCiConfig = PSCiConfig
  { psciLoadedFiles         :: [FilePath]
  , psciNodeFlags           :: [String]
  , psciEnvironment         :: P.Environment
  } deriving Show

-- | The PSCI state.
--
-- Holds a list of imported modules, loaded files, and partial let bindings.
-- The let bindings are partial,
-- because it makes more sense to apply the binding to the final evaluated expression.
data PSCiState = PSCiState
  { psciImportedModules     :: [ImportedModule]
  , psciLetBindings         :: [P.Declaration]
  , psciLoadedExterns       :: [(P.Module, P.ExternsFile)]
  } deriving Show

initialPSCiState :: PSCiState
initialPSCiState = PSCiState [] [] []

-- | All of the data that is contained by an ImportDeclaration in the AST.
-- That is:
--
-- * A module name, the name of the module which is being imported
-- * An ImportDeclarationType which specifies whether there is an explicit
--   import list, a hiding list, or neither.
-- * If the module is imported qualified, its qualified name in the importing
--   module. Otherwise, Nothing.
--
type ImportedModule = (P.ModuleName, P.ImportDeclarationType, Maybe P.ModuleName)

psciImportedModuleNames :: PSCiState -> [P.ModuleName]
psciImportedModuleNames (PSCiState{psciImportedModules = is}) =
  map (\(mn, _, _) -> mn) is

allImportsOf :: P.Module -> PSCiState -> [ImportedModule]
allImportsOf m (PSCiState{psciImportedModules = is}) =
  filter isImportOfThis is
  where
  name = P.getModuleName m
  isImportOfThis (name', _, _) = name == name'

-- * State helpers

-- | Updates the imported modules in the state record.
updateImportedModules :: ([ImportedModule] -> [ImportedModule]) -> PSCiState -> PSCiState
updateImportedModules f st = st { psciImportedModules = f (psciImportedModules st) }

-- | Updates the loaded externs files in the state record.
updateLoadedExterns :: ([(P.Module, P.ExternsFile)] -> [(P.Module, P.ExternsFile)]) -> PSCiState -> PSCiState
updateLoadedExterns f st = st { psciLoadedExterns = f (psciLoadedExterns st) }

-- | Updates the let bindings in the state record.
updateLets :: ([P.Declaration] -> [P.Declaration]) -> PSCiState -> PSCiState
updateLets f st = st { psciLetBindings = f (psciLetBindings st) }

-- * Commands

-- |
-- Valid Meta-commands for PSCI
--
data Command
  -- |
  -- A purescript expression
  --
  = Expression P.Expr
  -- |
  -- Show the help (ie, list of directives)
  --
  | ShowHelp
  -- |
  -- Import a module from a loaded file
  --
  | Import ImportedModule
  -- |
  -- Browse a module
  --
  | BrowseModule P.ModuleName
  -- |
  -- Exit PSCI
  --
  | QuitPSCi
  -- |
  -- Reset the state of the REPL
  --
  | ResetState
  -- |
  -- Add some declarations to the current evaluation context.
  --
  | Decls [P.Declaration]
  -- |
  -- Find the type of an expression
  --
  | TypeOf P.Expr
  -- |
  -- Find the kind of an expression
  --
  | KindOf P.Type
  -- |
  -- Shows information about the current state of the REPL
  --
  | ShowInfo ReplQuery

data ReplQuery
  = QueryLoaded
  | QueryImport
  deriving (Eq, Show)

-- | A list of all ReplQuery values.
replQueries :: [ReplQuery]
replQueries = [QueryLoaded, QueryImport]

replQueryStrings :: [String]
replQueryStrings = map showReplQuery replQueries

showReplQuery :: ReplQuery -> String
showReplQuery QueryLoaded = "loaded"
showReplQuery QueryImport = "import"

parseReplQuery :: String -> Maybe ReplQuery
parseReplQuery "loaded" = Just QueryLoaded
parseReplQuery "import" = Just QueryImport
parseReplQuery _ = Nothing

data Directive
  = Help
  | Quit
  | Reset
  | Browse
  | Type
  | Kind
  | Show
  deriving (Eq, Show)
