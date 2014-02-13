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
import Language.PureScript.TypeChecker as P
import Language.PureScript.Pretty as P
import Language.PureScript.Sugar as P
import Language.PureScript.Options as P
import Language.PureScript.ModuleDependencies as P
import Language.PureScript.DeadCodeElimination as P

import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Control.Monad (when, forM)
import Control.Monad.State.Lazy
import Control.Applicative ((<$>))
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
--  * Regroup values to take into account new value dependencies introduced by elaboration
--
--  * Eliminate dead code
--
--  * Generate Javascript, and perform optimization passes.
--
--  * Pretty-print the generated Javascript
--
compile :: Options -> [Module] -> Either String (String, String, Environment)
compile opts ms = do
  sorted <- sortModules ms
  desugared <- desugar sorted
  (elaborated, env) <- runCheck $ forM desugared $ \(Module moduleName decls) -> do
    modify (\s -> s { checkCurrentModule = Just (ModuleName moduleName) })
    Module moduleName <$> typeCheckAll (ModuleName moduleName) decls
  regrouped <- createBindingGroupsModule . collapseBindingGroupsModule $ elaborated
  let entryPoint = optionsEntryPoint opts
  let elim = maybe regrouped (\ep -> eliminateDeadCode env ep regrouped) entryPoint
  let js = mapMaybe (flip (moduleToJs opts) env) elim
  let exts = intercalate "\n" . map (flip moduleToPs env) $ elim
  js' <- case () of
              _ | optionsRunMain opts -> do
                    when ((ModuleName (ProperName "Main"), Ident "main") `M.notMember` (names env)) $
                      Left "Main.main is undefined"
                    return $ js ++ [JSApp (JSAccessor "main" (JSAccessor "Main" (JSVar "_ps"))) []]
                | otherwise -> return js
  return (prettyPrintJS [(wrapExportsContainer opts js')], exts, env)
