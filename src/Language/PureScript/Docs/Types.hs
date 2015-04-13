
module Language.PureScript.Docs.Types
  ( module Language.PureScript.Docs.Types
  , module ReExports
  )
  where

import Data.Version
import qualified Language.PureScript as P

import Language.PureScript.Docs.RenderedCode as ReExports (RenderedCode, ContainingModule(..))

data RenderedPackage = RenderedPackage
  { rpName    :: String
  , rpVersion :: Version
  , rpModules :: [RenderedModule]
  }
  deriving (Show)

data RenderedModule = RenderedModule
  { rmName         :: String
  , rmComments     :: Maybe String
  , rmDeclarations :: [RenderedDeclaration]
  }
  deriving (Show)

data RenderedDeclaration = RenderedDeclaration
  { rdTitle      :: String
  , rdComments   :: Maybe String
  , rdCode       :: RenderedCode
  , rdSourceSpan :: Maybe P.SourceSpan
  , rdChildren   :: [RenderedChildDeclaration]
  }
  deriving (Show)

data RenderedChildDeclaration = RenderedChildDeclaration
  { rcdTitle      :: String
  , rcdComments   :: Maybe String
  , rcdCode       :: RenderedCode
  , rcdSourceSpan :: Maybe P.SourceSpan
  , rcdType       :: RenderedChildDeclarationType
  }
  deriving (Show)

data RenderedChildDeclarationType
  = ChildInstance
  | ChildDataConstructor
  | ChildTypeClassMember
  deriving (Show, Eq, Ord)
