-- |
-- Type declarations and associated basic functions for PSCI.
--
module PSCi.Types where

import Prelude ()
import Prelude.Compat

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Language.PureScript as P

data PSCiOptions = PSCiOptions
  { psciMultiLineMode     :: Bool
  , psciInputFile         :: [FilePath]
  , psciForeignInputFiles :: [FilePath]
  , psciInputNodeFlags    :: [String]
  }

-- |
-- The PSCI state.
-- Holds a list of imported modules, loaded files, and partial let bindings.
-- The let bindings are partial,
-- because it makes more sense to apply the binding to the final evaluated expression.
--
data PSCiState = PSCiState
  { psciImportedModules     :: [ImportedModule]
  , psciLoadedFiles         :: [FilePath]
  , psciLoadedExterns       :: [(P.Module, P.ExternsFile)]
  , psciForeignFiles        :: Map P.ModuleName FilePath
  , psciLetBindings         :: [P.Declaration]
  , psciNodeFlags           :: [String]
  , psciEnvironment         :: P.Environment
  } deriving Show

initialPSCiState :: PSCiState
initialPSCiState = PSCiState [] [] [] Map.empty [] [] P.initEnvironment

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

-- State helpers

-- |
-- Updates the state to have more imported modules.
--
updateImportedModules :: ImportedModule -> PSCiState -> PSCiState
updateImportedModules im st = st { psciImportedModules = im : psciImportedModules st }

-- | Updates the state to have more loaded .purs files.
updateLoadedFiles :: [FilePath] -> PSCiState -> PSCiState
updateLoadedFiles paths st = st { psciLoadedFiles = psciLoadedFiles st ++ paths }

-- | Updates the state to have more loaded externs files.
updateLoadedExterns :: [(P.Module, P.ExternsFile)] -> PSCiState -> PSCiState
updateLoadedExterns externs st = st { psciLoadedExterns = psciLoadedExterns st ++ externs }

-- |
-- Updates the state to have more let bindings.
--
updateLets :: [P.Declaration] -> PSCiState -> PSCiState
updateLets ds st = st { psciLetBindings = psciLetBindings st ++ ds }

-- |
-- Updates the state to have more let bindings.
--
updateForeignFiles :: Map P.ModuleName FilePath -> PSCiState -> PSCiState
updateForeignFiles fs st = st { psciForeignFiles = psciForeignFiles st `Map.union` fs }

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
