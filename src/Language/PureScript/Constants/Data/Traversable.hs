module Language.PureScript.Constants.Data.Traversable where

import Data.String (IsString)
import Language.PureScript.Names

traverse :: forall a. (IsString a) => a
traverse = "traverse"

sequence :: forall a. (IsString a) => a
sequence = "sequence"

pattern DataTraversable :: ModuleName
pattern DataTraversable = ModuleName "Data.Traversable"

pattern Traversable :: Qualified (ProperName 'ClassName)
pattern Traversable = Qualified (ByModuleName DataTraversable) (ProperName "Traversable")

identTraverse :: Qualified Ident
identTraverse = Qualified (ByModuleName DataTraversable) (Ident traverse)
