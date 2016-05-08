-- |
-- The main compiler module
--
module Language.PureScript
  ( module P
  , version
  ) where


import Control.Monad.Supply as P

import Data.Version (Version)

import Language.PureScript.AST as P
import Language.PureScript.Comments as P
import Language.PureScript.Crash as P
import Language.PureScript.Environment as P
import Language.PureScript.Errors as P hiding (indent)
import Language.PureScript.Externs as P
import Language.PureScript.Kinds as P
import Language.PureScript.Linter as P
import Language.PureScript.Make as P
import Language.PureScript.ModuleDependencies as P
import Language.PureScript.Names as P
import Language.PureScript.Options as P
import Language.PureScript.Parser as P
import Language.PureScript.Pretty as P
import Language.PureScript.Renamer as P
import Language.PureScript.Sugar as P
import Language.PureScript.TypeChecker as P
import Language.PureScript.Types as P

import qualified Paths_purescript as Paths

version :: Version
version = Paths.version
