{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Functions for converting PureScript ASTs into values of the data types
-- from Language.PureScript.Docs.

module Language.PureScript.Docs.Convert
  ( convertModules
  , convertModulesInPackage
  , collectBookmarks
  ) where

import Prelude ()
import Prelude.Compat

import Control.Monad.Error.Class (MonadError)
import Control.Arrow ((&&&))
import Control.Category ((>>>))
import qualified Data.Map as Map

import qualified Language.PureScript as P

import Language.PureScript.Docs.Types
import Language.PureScript.Docs.Convert.Single (convertSingleModule, collectBookmarks)
import Language.PureScript.Docs.Convert.ReExports (updateReExports)

-- |
-- Like convertModules, except that it takes a list of modules, together with
-- their dependency status, and discards dependency modules in the resulting
-- documentation.
--
convertModulesInPackage ::
  (Functor m, MonadError P.MultipleErrors m) =>
  P.Env ->
  [InPackage P.Module] ->
  m [Module]
convertModulesInPackage env modules =
  go modules
  where
  localNames =
    map (P.runModuleName . P.getModuleName) (takeLocals modules)
  go =
    map ignorePackage
     >>> convertModules env
     >>> fmap (filter ((`elem` localNames) . modName))

-- |
-- Convert a group of modules to the intermediate format, designed for
-- producing documentation from. It is also necessary to pass an Env containing
-- imports/exports information about the list of modules, which is needed for
-- documenting re-exports.
--
-- Preconditions:
--
--     * If any module in the list re-exports documentation from other
--     modules, those modules must also be included in the list.
--     * The modules passed must have had names desugared and re-exports
--     elaborated first.
--
-- If either of these are not satisfied, an internal error will be thrown. To
-- avoid this, it is recommended to use
-- Language.PureScript.Docs.ParseAndDesugar to construct the inputs to this
-- function.
--
convertModules ::
  (Functor m, MonadError P.MultipleErrors m) =>
  P.Env ->
  [P.Module] ->
  m [Module]
convertModules env =
  P.sortModules >>> fmap (convertSorted env . fst)

-- |
-- Convert a sorted list of modules.
--
convertSorted :: P.Env -> [P.Module] -> [Module]
convertSorted env modules =
  let
    traversalOrder =
      map P.getModuleName modules
    moduleMap =
      Map.fromList $ map (P.getModuleName &&& convertSingleModule) modules
  in
    Map.elems (updateReExports env traversalOrder moduleMap)
