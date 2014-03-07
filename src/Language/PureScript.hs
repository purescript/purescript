-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- The main compiler module
--
-----------------------------------------------------------------------------

module Language.PureScript (module P, compile) where

import Language.PureScript.Values as P
import Language.PureScript.Types as P
import Language.PureScript.Kinds as P
import Language.PureScript.Declarations as P
import Language.PureScript.Names as P
import Language.PureScript.Parser as P
import Language.PureScript.CodeGen as P
import Language.PureScript.CodeGen.Common as P
import Language.PureScript.TypeChecker as P
import Language.PureScript.Pretty as P
import Language.PureScript.Sugar as P
import Language.PureScript.Options as P
import Language.PureScript.ModuleDependencies as P
import Language.PureScript.DeadCodeElimination as P

import qualified Language.PureScript.Constants as C

import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Control.Monad.State.Lazy
import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.Map as M

-- |
-- Compile a collection of modules
--
-- The compilation pipeline proceeds as follows:
--
--  * Sort the modules based on module dependencies, checking for cyclic dependencies.
--
--  * Perform a set of desugaring passes.
--
--  * Type check, and elaborate values to include type annotations and type class dictionaries.
--
--  * Regroup values to take into account new value dependencies introduced by elaboration.
--
--  * Eliminate dead code.
--
--  * Generate Javascript, and perform optimization passes.
--
--  * Pretty-print the generated Javascript
--
compile :: Options -> [Module] -> Either String (String, String, Environment)
compile opts ms = do
  sorted <- sortModules ms
  desugared <- desugar sorted
  (elaborated, env) <- runCheck $ forM desugared $ \(Module moduleName' decls exps) -> do
    modify (\s -> s { checkCurrentModule = Just moduleName' })
    Module moduleName' <$> typeCheckAll mainModuleIdent moduleName' decls <*> pure exps
  regrouped <- createBindingGroupsModule . collapseBindingGroupsModule $ elaborated
  let entryPoints = moduleNameFromString `map` optionsModules opts
  let elim = if null entryPoints then regrouped else eliminateDeadCode entryPoints regrouped
  let js = mapMaybe (flip (moduleToJs opts) env) elim
  let exts = intercalate "\n" . map (`moduleToPs` env) $ elim
  js' <- case mainModuleIdent of
    Just mmi -> do
      when ((mmi, Ident C.main) `M.notMember` names env) $
        Left $ show mmi ++ "." ++ C.main ++ " is undefined"
      return $ js ++ [JSApp (JSAccessor C.main (JSAccessor (moduleNameToJs mmi) (JSVar C._ps))) []]
    _ -> return js
  return (prettyPrintJS [wrapExportsContainer opts js'], exts, env)
  where
  mainModuleIdent = moduleNameFromString <$> optionsMain opts
