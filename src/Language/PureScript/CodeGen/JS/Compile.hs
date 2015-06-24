-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.JS.Compile
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.CodeGen.JS.Compile where

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Supply (evalSupplyT)
import Control.Monad.Supply.Class (MonadSupply())
import Control.Monad.Reader
import Control.Monad.Writer

import qualified Data.Map as M
import qualified Data.Traversable as T (traverse)
import qualified Language.PureScript as P
import qualified Language.PureScript.CodeGen.JS as J
import qualified Language.PureScript.Constants as C
import qualified Language.PureScript.Core as CR
import qualified Language.PureScript.CoreImp as CI

compileJS :: forall m. (Functor m, Applicative m, MonadError P.MultipleErrors m, MonadWriter P.MultipleErrors m, MonadReader (P.Options P.Compile) m)
          => [P.Module] -> M.Map P.ModuleName (FilePath, P.ForeignJS) -> [String] -> m (String, String)
compileJS ms foreigns prefix = do
  (modulesToCodeGen, env, nextVar, exts) <- P.compile ms
  js <- concat <$> evalSupplyT nextVar (T.traverse (CI.moduleToCoreImp >=> codegenModule) modulesToCodeGen)
  js' <- generateMain env js
  let pjs = unlines $ map ("// " ++) prefix ++ [P.prettyPrintJS js']
  return (pjs, exts)

  where

  codegenModule :: (Functor n, Applicative n, MonadError P.MultipleErrors n, MonadWriter P.MultipleErrors n, MonadReader (P.Options P.Compile) n, MonadSupply n)
                => CR.Module (CI.Decl CR.Ann) -> n [J.JS]
  codegenModule m =
    let requiresForeign = not $ null (CR.moduleForeign m)
        mn = CR.moduleName m
    in case mn `M.lookup` foreigns of
      Just (path, js)
        | not requiresForeign -> do
            tell $ P.errorMessage $ P.UnnecessaryFFIModule mn path
            J.moduleToJs m Nothing
        | otherwise -> J.moduleToJs m $ Just $
            J.JSApp (J.JSFunction Nothing [] $
                      J.JSBlock [ J.JSVariableIntroduction "exports" (Just $ J.JSObjectLiteral [])
                                , J.JSRaw js
                                , J.JSReturn (J.JSVar "exports")
                                ]) []
      Nothing | requiresForeign -> throwError . P.errorMessage $ P.MissingFFIModule mn
              | otherwise -> J.moduleToJs m Nothing

  generateMain :: P.Environment -> [J.JS] -> m [J.JS]
  generateMain env js = do
    mainName <- asks P.optionsMain
    additional <- asks P.optionsAdditional
    case P.moduleNameFromString <$> mainName of
      Just mmi -> do
        when ((mmi, P.Ident C.main) `M.notMember` P.names env) $
          throwError . P.errorMessage $ P.NameIsUndefined (P.Ident C.main)
        return $ js ++ [J.mainCall mmi (P.browserNamespace additional)]
      _ -> return js
