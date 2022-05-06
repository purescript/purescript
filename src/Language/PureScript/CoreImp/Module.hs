module Language.PureScript.CoreImp.Module where

import Protolude
import qualified Data.List.NonEmpty as NEL (NonEmpty)

import Language.PureScript.Comments
import Language.PureScript.CoreImp.AST
import Language.PureScript.PSString (PSString)

data Module = Module
  { modHeader :: [Comment]
  , modImports :: [Import]
  , modBody :: [AST]
  , modExports :: [Export]
  }

data Import = Import Text PSString

data Export = Export (NEL.NonEmpty Text) (Maybe PSString)
