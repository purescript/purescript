module Language.PureScript.Constants.Data.Generic.Rep where

import Prelude.Compat
import Language.PureScript.Names

pattern DataGenericRep :: ModuleName
pattern DataGenericRep = ModuleName "Data.Generic.Rep"

pattern Generic :: Qualified (ProperName 'ClassName)
pattern Generic = Qualified (Just DataGenericRep) (ProperName "Generic")

to :: Qualified Ident
to = Qualified (Just DataGenericRep) (Ident "to")

from :: Qualified Ident
from = Qualified (Just DataGenericRep) (Ident "from")

pattern NoConstructors :: Qualified (ProperName a)
pattern NoConstructors = Qualified (Just DataGenericRep) (ProperName "NoConstructors")

pattern NoArguments :: Qualified (ProperName a)
pattern NoArguments = Qualified (Just DataGenericRep) (ProperName "NoArguments")

pattern Sum :: Qualified (ProperName a)
pattern Sum = Qualified (Just DataGenericRep) (ProperName "Sum")

pattern Inl :: Qualified (ProperName a)
pattern Inl = Qualified (Just DataGenericRep) (ProperName "Inl")

pattern Inr :: Qualified (ProperName a)
pattern Inr = Qualified (Just DataGenericRep) (ProperName "Inr")

pattern Product :: Qualified (ProperName a)
pattern Product = Qualified (Just DataGenericRep) (ProperName "Product")

pattern Constructor :: Qualified (ProperName a)
pattern Constructor = Qualified (Just DataGenericRep) (ProperName "Constructor")

pattern Argument :: Qualified (ProperName a)
pattern Argument = Qualified (Just DataGenericRep) (ProperName "Argument")
