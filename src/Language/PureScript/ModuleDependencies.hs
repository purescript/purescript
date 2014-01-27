-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.ModuleDependencies
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | Provides the ability to sort modules based on module dependencies
--
-----------------------------------------------------------------------------

module Language.PureScript.ModuleDependencies (
  sortModules
) where

import Data.Data
import Data.Graph
import Data.Generics
import Data.List (nub, intersect)
import Control.Applicative ((<$>))

import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Values
import Language.PureScript.Types

-- |
-- Sort a collection of modules based on module dependencies.
--
-- Reports an error if the module graph contains a cycle.
--
sortModules :: [Module] -> Either String [Module]
sortModules ms = do
  let verts = map (\m -> (m, getModuleName m, usedModules m)) ms
  mapM toModule $ stronglyConnComp verts

-- |
-- Calculate a list of used modules based on explicit imports and qualified names
--
usedModules :: (Data d) => d -> [ProperName]
usedModules = nub . everything (++) (mkQ [] qualifiedIdents `extQ` qualifiedProperNames `extQ` imports)
  where
  qualifiedIdents :: Qualified Ident -> [ProperName]
  qualifiedIdents (Qualified (Just (ModuleName pn)) _) = [pn]
  qualifiedIdents _ = []
  qualifiedProperNames :: Qualified ProperName -> [ProperName]
  qualifiedProperNames (Qualified (Just (ModuleName pn)) _) = [pn]
  qualifiedProperNames _ = []
  imports :: Declaration -> [ProperName]
  imports (ImportDeclaration (ModuleName pn) _) = [pn]
  imports _ = []

getModuleName :: Module -> ProperName
getModuleName (Module pn _) = pn

-- |
-- Convert a strongly connected component of the module graph to a module
--
toModule :: SCC Module -> Either String Module
toModule (AcyclicSCC m) = return m
toModule (CyclicSCC [m]) = return m
toModule (CyclicSCC _) = Left "Cycle in module dependencies"
