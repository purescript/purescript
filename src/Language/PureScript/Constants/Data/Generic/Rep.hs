module Language.PureScript.Constants.Data.Generic.Rep where

import Language.PureScript.Names

pattern DataGenericRep :: ModuleName
pattern DataGenericRep = ModuleName "Data.Generic.Rep"

pattern Generic :: Qualified (ProperName 'ClassName)
pattern Generic = Qualified (ByModuleName DataGenericRep) (ProperName "Generic")

to :: Qualified Ident
to = Qualified (ByModuleName DataGenericRep) (Ident "to")

from :: Qualified Ident
from = Qualified (ByModuleName DataGenericRep) (Ident "from")

pattern NoConstructors :: Qualified (ProperName a)
pattern NoConstructors = Qualified (ByModuleName DataGenericRep) (ProperName "NoConstructors")

pattern NoArguments :: Qualified (ProperName a)
pattern NoArguments = Qualified (ByModuleName DataGenericRep) (ProperName "NoArguments")

pattern Sum :: Qualified (ProperName a)
pattern Sum = Qualified (ByModuleName DataGenericRep) (ProperName "Sum")

pattern Inl :: Qualified (ProperName a)
pattern Inl = Qualified (ByModuleName DataGenericRep) (ProperName "Inl")

pattern Inr :: Qualified (ProperName a)
pattern Inr = Qualified (ByModuleName DataGenericRep) (ProperName "Inr")

pattern Product :: Qualified (ProperName a)
pattern Product = Qualified (ByModuleName DataGenericRep) (ProperName "Product")

pattern Constructor :: Qualified (ProperName a)
pattern Constructor = Qualified (ByModuleName DataGenericRep) (ProperName "Constructor")

pattern Argument :: Qualified (ProperName a)
pattern Argument = Qualified (ByModuleName DataGenericRep) (ProperName "Argument")
