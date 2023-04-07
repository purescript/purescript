module Language.PureScript.CoreImp.Module where

import Protolude
import Data.List.NonEmpty qualified as NEL (NonEmpty)

import Language.PureScript.Comments (Comment)
import Language.PureScript.CoreImp.AST (AST)
import Language.PureScript.PSString (PSString)

data Module = Module
  { modHeader :: [Comment]
  , modImports :: [Import]
  , modBody :: [AST]
  , modExports :: [Export]
  }

data Import = Import Text PSString

data Export = Export (NEL.NonEmpty Text) (Maybe PSString)
