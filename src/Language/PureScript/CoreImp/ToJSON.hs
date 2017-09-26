{-# LANGUAGE NoOverloadedStrings #-}
-- |
-- Dump the core imperative representation in JSON format for consumption
-- by third-party code generators
--
module Language.PureScript.CoreImp.ToJSON
  ( moduleToJSON
  ) where


import Prelude.Compat

import Data.Aeson
import Data.Version (Version, showVersion)
import qualified Data.Text as T

import Language.PureScript.CoreFn
import Language.PureScript.Names

import Language.PureScript.CoreImp.AST (AST)



identToJSON :: Ident -> Value
identToJSON = toJSON . runIdent

moduleNameToJSON :: ModuleName -> Value
moduleNameToJSON = toJSON . runModuleName


astToJSON :: AST -> Value
astToJSON = toJSON

moduleToJSON :: Version -> Module a -> [AST] -> Value
moduleToJSON v m a = object [ T.pack "imports"   .= map (moduleNameToJSON . snd) (moduleImports m)
                            , T.pack "exports"   .= map identToJSON (moduleExports m)
                            , T.pack "foreign"   .= map (identToJSON . fst) (moduleForeign m)
                            , T.pack "builtWith" .= toJSON (showVersion v)
                            , T.pack "ast"       .= map (astToJSON) a
                            ]
