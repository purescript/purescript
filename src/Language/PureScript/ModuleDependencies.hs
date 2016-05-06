-- |
-- Provides the ability to sort modules based on module dependencies
--
module Language.PureScript.ModuleDependencies
  ( sortModules
  , ModuleGraph
  ) where

import Prelude.Compat

import Control.Monad.Error.Class (MonadError(..))

import Data.Graph
import Data.List (nub)
import Data.Maybe (fromMaybe)

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.Types

-- | A list of modules with their transitive dependencies
type ModuleGraph = [(ModuleName, [ModuleName])]

-- | Sort a collection of modules based on module dependencies.
--
-- Reports an error if the module graph contains a cycle.
--
sortModules :: (MonadError MultipleErrors m) => [Module] -> m ([Module], ModuleGraph)
sortModules ms = do
  let verts = map goModule ms
  ms' <- mapM toModule $ stronglyConnComp verts
  let (graph, fromVertex, toVertex) = graphFromEdges verts
      moduleGraph = do (_, mn, _) <- verts
                       let v       = fromMaybe (internalError "sortModules: vertex not found") (toVertex mn)
                           deps    = reachable graph v
                           toKey i = case fromVertex i of (_, key, _) -> key
                       return (mn, filter (/= mn) (map toKey deps))
  return (ms', moduleGraph)
  where
  goModule :: Module -> (Module, ModuleName, [ModuleName])
  goModule m@(Module _ _ _ ds _) =
    let ams = concatMap extractQualAs ds
    in (m, getModuleName m, nub (concatMap (usedModules ams) ds))

  -- Extract module names that have been brought into scope by an `as` import.
  extractQualAs :: Declaration -> [ModuleName]
  extractQualAs (PositionedDeclaration _ _ d) = extractQualAs d
  extractQualAs (ImportDeclaration _ _ (Just am)) = [am]
  extractQualAs _ = []

-- |
-- Calculate a list of used modules based on explicit imports and qualified
-- names. `ams` is a list of `ModuleNames` that refer to names brought into
-- scope by importing with `as` - this ensures that when building the list we
-- don't inadvertantly assume a dependency on an actual module, if there is a
-- module that has the same name as the qualified import.
--
usedModules :: [ModuleName] -> Declaration -> [ModuleName]
usedModules ams d =
  let (f, _, _, _, _) = everythingOnValues (++) forDecls forValues (const []) (const []) (const [])
      (g, _, _, _, _) = accumTypes (everythingOnTypes (++) forTypes)
  in nub (f d ++ g d)
  where

  forDecls :: Declaration -> [ModuleName]
  forDecls (ImportDeclaration mn _ _) =
    -- Regardless of whether an imported module is qualified we still need to
    -- take into account its import to build an accurate list of dependencies.
    [mn]
  forDecls (FixityDeclaration fd)
    | Just mn <- extractQualFixity fd, mn `notElem` ams = [mn]
  forDecls (TypeInstanceDeclaration _ _ (Qualified (Just mn) _) _ _)
    | mn `notElem` ams = [mn]
  forDecls _ = []

  forValues :: Expr -> [ModuleName]
  forValues (Var (Qualified (Just mn) _))
    | mn `notElem` ams = [mn]
  forValues (Constructor (Qualified (Just mn) _))
    | mn `notElem` ams = [mn]
  forValues _ = []

  forTypes :: Type -> [ModuleName]
  forTypes (TypeConstructor (Qualified (Just mn) _))
    | mn `notElem` ams = [mn]
  forTypes _ = []

  extractQualFixity :: Either ValueFixity TypeFixity -> Maybe ModuleName
  extractQualFixity (Left (ValueFixity _ (Qualified mn _) _)) = mn
  extractQualFixity (Right (TypeFixity _ (Qualified mn _) _)) = mn

-- |
-- Convert a strongly connected component of the module graph to a module
--
toModule :: (MonadError MultipleErrors m) => SCC Module -> m Module
toModule (AcyclicSCC m) = return m
toModule (CyclicSCC [m]) = return m
toModule (CyclicSCC ms) = throwError . errorMessage $ CycleInModules (map getModuleName ms)
