module Language.PureScript.Ide.Filter.Imports where


import Protolude                     hiding (isPrefixOf)

import Language.PureScript.Ide.Types (IdeDataConstructor(..), IdeDeclaration(..), IdeDeclarationAnn(..), IdeType(..), IdeTypeClass(..), IdeTypeOperator(..), IdeTypeSynonym(..), IdeValue(..), IdeValueOperator(..))
import Language.PureScript.Ide.Imports (Import(..))

import Language.PureScript qualified as P

matchImport :: Maybe P.ModuleName -> P.ModuleName -> IdeDeclarationAnn -> Import -> Bool
matchImport matchQualifier declMod (IdeDeclarationAnn _ decl) (Import importMod declTy qualifier) | declMod == importMod && matchQualifier == qualifier =
  case declTy of 
    P.Implicit -> True
    P.Explicit refs -> any (matchRef decl) refs
    P.Hiding refs -> not $ any (matchRef decl) refs
  where
    matchRef (IdeDeclValue (IdeValue ident _)) (P.ValueRef _ ident') = ident == ident'
    matchRef (IdeDeclType (IdeType tname _kind _dctors)) (P.TypeRef _ tname' _dctors') = tname == tname'
    matchRef (IdeDeclTypeSynonym (IdeTypeSynonym tname _type _kind)) (P.TypeRef _ tname' _dctors) = tname == tname' -- Can this occur?
    
    matchRef (IdeDeclDataConstructor (IdeDataConstructor dcname tname _type)) (P.TypeRef _ tname' dctors) =
      tname == tname'
      && maybe True (dcname `elem`) dctors -- (..) or explicitly lists constructor
      
    matchRef (IdeDeclTypeClass (IdeTypeClass tcname _kind _instances)) (P.TypeClassRef _ tcname') = tcname == tcname'
    matchRef (IdeDeclValueOperator (IdeValueOperator{ _ideValueOpName })) (P.ValueOpRef _ opname) = _ideValueOpName == opname
    matchRef (IdeDeclTypeOperator (IdeTypeOperator{ _ideTypeOpName })) (P.TypeOpRef _ opname) = _ideTypeOpName == opname
    matchRef _ _ = False

matchImport _ _ _ _ = False
