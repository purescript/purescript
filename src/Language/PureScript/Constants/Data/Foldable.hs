module Language.PureScript.Constants.Data.Foldable where

import Data.String (IsString)
import Language.PureScript.Names

foldl :: forall a. (IsString a) => a
foldl = "foldl"

foldr :: forall a. (IsString a) => a
foldr = "foldr"

foldMap :: forall a. (IsString a) => a
foldMap = "foldMap"

pattern DataFoldable :: ModuleName
pattern DataFoldable = ModuleName "Data.Foldable"

pattern Foldable :: Qualified (ProperName 'ClassName)
pattern Foldable = Qualified (ByModuleName DataFoldable) (ProperName "Foldable")

identFoldl :: Qualified Ident
identFoldl = Qualified (ByModuleName DataFoldable) (Ident foldl)

identFoldr :: Qualified Ident
identFoldr = Qualified (ByModuleName DataFoldable) (Ident foldr)

identFoldMap :: Qualified Ident
identFoldMap = Qualified (ByModuleName DataFoldable) (Ident foldMap)
