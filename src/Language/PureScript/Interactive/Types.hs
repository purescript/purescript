{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Type declarations and associated basic functions for PSCI.
--
module Language.PureScript.Interactive.Types where

import Prelude.Compat

import           Control.Applicative (liftA2)
import           Control.Monad (liftM2)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Reader.Class (MonadReader(..))
import           Control.Monad.State.Class (MonadState(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import qualified Language.PureScript as P

-- | The PSCI configuration.
--
-- These configuration values do not change during execution.
--
data PSCiConfig = PSCiConfig
  { psciLoadedFiles         :: [FilePath]
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
psciImportedModuleNames PSCiState{psciImportedModules = is} =
  map (\(mn, _, _) -> mn) is

allImportsOf :: P.Module -> PSCiState -> [ImportedModule]
allImportsOf m PSCiState{psciImportedModules = is} =
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
  -- | A purescript expression
  = Expression P.Expr
  -- | Show the help (ie, list of directives)
  | ShowHelp
  -- | Import a module from a loaded file
  | Import ImportedModule
  -- | Browse a module
  | BrowseModule P.ModuleName
  -- | Exit PSCI
  | QuitPSCi
  -- | Reload all the imported modules of the REPL
  | ReloadState
  -- | Clear the state of the REPL
  | ClearState
  -- | Add some declarations to the current evaluation context
  | Decls [P.Declaration]
  -- | Find the type of an expression
  | TypeOf P.Expr
  -- | Find the kind of an expression
  | KindOf P.Type
  -- | Shows information about the current state of the REPL
  | ShowInfo ReplQuery
  -- | Paste multiple lines
  | PasteLines
  deriving Show

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
  | Reload
  | Clear
  | Browse
  | Type
  | Kind
  | Show
  | Paste
  deriving (Eq, Show)

-- | A list of functions used in PSCi
data PSCiFns = PSCiFns { _evalJS :: String -> IO ()
                       , _reload :: IO ()
                       , _print :: String -> IO ()
                       }

-- | A monad transformer to use PSCi functions
newtype PSCiT m a = PSCiT { runPSCiT :: PSCiFns -> m a }

instance Functor f => Functor (PSCiT f) where
  fmap f (PSCiT m) = PSCiT $ fmap (fmap f) m

instance Applicative f => Applicative (PSCiT f) where
  pure = PSCiT . const . pure
  PSCiT f <*> PSCiT a = PSCiT $ liftA2 (<*>) f a

instance Monad m => Monad (PSCiT m) where
  return = pure
  PSCiT m >>= f = PSCiT $ liftM2 (>>=) m $ flip (runPSCiT . f)

instance MonadTrans PSCiT where
  lift = PSCiT . const

instance MonadIO m => MonadIO (PSCiT m) where
  liftIO = lift . liftIO

instance MonadReader r m => MonadReader r (PSCiT m) where
  ask = lift ask
  local f = PSCiT . fmap (local f) . runPSCiT

instance MonadState s m => MonadState s (PSCiT m) where
  state = lift . state

-- | A monadic type class for PSCiT
class MonadPSCi m where
  psciEvalJS :: String -> m ()
  psciReload :: m ()
  psciPrint :: String -> m ()

instance MonadIO m => MonadPSCi (PSCiT m) where
  psciEvalJS js = PSCiT $ \ (PSCiFns e _ _) -> liftIO $ e js
  psciReload = PSCiT $ \ (PSCiFns _ r _) -> liftIO r
  psciPrint str = PSCiT $ \ (PSCiFns _ _ p) -> liftIO $ p str
