-- |
-- Functions for updating source file path in the build artifacts.
module Language.PureScript.Make.Patch
  ( patchExterns
  , patchWarnings
  , patchDocsModule
  , patchCoreFnJSON
  , patchSourceMapJSON
  ) where

import Prelude

import Control.Lens.Extras (template)
import Control.Lens (over)
import Data.Aeson (Value(Object, String, Array))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Key qualified as Key
import Data.Data (Data)
import Language.PureScript.Externs (ExternsFile)
import Language.PureScript.AST (modifySpanName)
import Language.PureScript.AST.Declarations.ChainId (ChainId (..))
import Language.PureScript.Errors (MultipleErrors)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Functor.Identity (Identity(..))
import Language.PureScript.Docs.Types qualified as Docs

patchSpans :: forall a. Data a => (String -> String) -> a -> a
patchSpans = over template . modifySpanName

patchChainIds :: (String -> String) -> ExternsFile -> ExternsFile
patchChainIds = over template . patchChainId :: (String -> String) -> ExternsFile -> ExternsFile

patchChainId :: (String -> String) -> ChainId -> ChainId
patchChainId f (ChainId (fp, pos)) = ChainId (f fp, pos)

-- Source file names appear in externs both in source spans and inside
-- instance chain ids, so both have to be patched.
patchExterns :: (String -> String) -> ExternsFile -> ExternsFile
patchExterns f = patchChainIds f . patchSpans f

patchWarnings :: (String -> String) -> MultipleErrors -> MultipleErrors
patchWarnings f = over template (modifySpanName f) :: MultipleErrors -> MultipleErrors

patchJSONKey :: String -> (String -> String) -> Value -> Value
patchJSONKey key modStr (Object km) =
    Object $ runIdentity $ KeyMap.alterF modifyVal (Key.fromText (T.pack key)) km
  where
    modText = T.pack . modStr . T.unpack
    modString (String t)= String $ modText t
    modString other = other
    modifyVal (Just (String t)) = Identity (Just (String (modText t)))
    modifyVal (Just (Array arr)) = Identity (Just (Array $ V.map modString arr))
    modifyVal old = Identity old
patchJSONKey _ _ json = json

patchCoreFnJSON :: (String -> String) -> Value -> Value
patchCoreFnJSON = patchJSONKey "modulePath"

patchDocsModule :: (String -> String) -> Docs.Module -> Docs.Module
patchDocsModule modStr m =
  m
    { Docs.modDeclarations = map patchDeclaration $ Docs.modDeclarations m
    , Docs.modReExports = map (fmap (map patchDeclaration)) $ Docs.modReExports m
    }
  where
    patchDeclaration = patchSpans modStr

patchSourceMapJSON :: (String -> String) -> Value -> Value
patchSourceMapJSON modStr =
  patchJSONKey "sources" modStr