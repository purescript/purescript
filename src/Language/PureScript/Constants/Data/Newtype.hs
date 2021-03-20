module Language.PureScript.Constants.Data.Newtype where

import Prelude.Compat
import Language.PureScript.Names

pattern Newtype :: Qualified (ProperName 'ClassName)
pattern Newtype = Qualified (Just (ModuleName "Data.Newtype")) (ProperName "Newtype")
