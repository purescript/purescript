module Language.PureScript.Ide.Usage where

import Protolude hiding (moduleName)

import qualified Data.Map as Map
import Language.PureScript.Ide.Types
import Language.PureScript.Ide.Util

import qualified Language.PureScript as P


{-
How we find usages, given an IdeDeclaration:
1. Find all modules that reexport the given declaration
2. Find all modules that import from those modules, and while traversing the
imports build a specification for how the identifier can be found in the
module.
-}

-- | (Qualifier, Namespace, Identifier)
type Search = (Maybe P.ModuleName, IdeNamespace, Text)

findReexportingModules
  :: (P.ModuleName, IdeDeclaration)
  -- ^ The declaration and the module it is defined in for which we are
  -- searching usages
  -> ModuleMap [IdeDeclarationAnn]
  -- ^ Our declaration cache. Needs to have reexports resolved
  -> [P.ModuleName]
  -- ^ All the modules that reexport the declaration. This does NOT include
  -- the defining module
findReexportingModules (moduleName, declaration) decls =
  Map.keys (Map.filter (any hasReexport) decls)
  where
    hasReexport d =
      (d & _idaDeclaration & identifierFromIdeDeclaration) == identifierFromIdeDeclaration declaration
      && (d & _idaAnnotation & _annExportedFrom) == Just moduleName
      && (d & _idaDeclaration & namespaceForDeclaration) == namespaceForDeclaration declaration

directDependants :: IdeDeclaration -> ModuleMap P.Module -> P.ModuleName -> ModuleMap (NonEmpty Search)
directDependants declaration modules mn = Map.mapMaybe (nonEmpty . go) modules
  where
    go :: P.Module -> [Search]
    go = foldMap isImporting . P.getModuleDeclarations

    isImporting d = case d of
      P.ImportDeclaration _ mn' it qual | mn == mn' -> case it of
        P.Implicit -> pure (qual, namespaceForDeclaration declaration, identifierFromIdeDeclaration declaration)
        P.Explicit refs
          | any (declaration `matchesRef`) refs ->
              pure (qual, namespaceForDeclaration declaration, identifierFromIdeDeclaration declaration)
        P.Explicit _ -> []
        P.Hiding refs
          | not (any (declaration `matchesRef`) refs) ->
              pure (qual, namespaceForDeclaration declaration, identifierFromIdeDeclaration declaration)
        P.Hiding _ -> []
      _ -> []

-- | Determines whether an IdeDeclaration is referenced by a DeclarationRef.
--
-- TODO(Christoph): We should also extract the spans of matching refs here,
-- since they also count as a usage (at least for rename refactorings)
matchesRef :: IdeDeclaration -> P.DeclarationRef -> Bool
matchesRef declaration ref = case declaration of
  IdeDeclValue valueDecl -> case ref of
    P.ValueRef _ i -> i == _ideValueIdent valueDecl
    _ -> False
  IdeDeclType typeDecl -> case ref of
    P.TypeRef _ tn _ -> tn == _ideTypeName typeDecl
    _ -> False
  IdeDeclTypeSynonym synonym -> case ref of
    P.TypeRef _ tn _ -> tn == _ideSynonymName synonym
    _ -> False
  IdeDeclDataConstructor dtor -> case ref of
    P.TypeRef _ tn dtors
    -- We check if the given data constructor constructs the type imported
    -- here.
    -- This way we match `Just` with an import like `import Data.Maybe (Maybe(..))`
      | _ideDtorTypeName dtor == tn ->
          maybe True (elem (_ideDtorName dtor)) dtors
    _ -> False
  IdeDeclTypeClass typeClass -> case ref of
    P.TypeClassRef _ name -> name == _ideTCName typeClass
    _ -> False
  IdeDeclValueOperator valueOperator -> case ref of
    P.ValueOpRef _ opName -> opName == _ideValueOpName valueOperator
    _ -> False
  IdeDeclTypeOperator typeOperator -> case ref of
    P.TypeOpRef _ opName -> opName == _ideTypeOpName typeOperator
    _ -> False
  IdeDeclKind kind -> case ref of
    P.KindRef _ kindName -> kindName == kind
    _ -> False

eligibleModules
  :: (P.ModuleName, IdeDeclaration)
  -> ModuleMap [IdeDeclarationAnn]
  -> ModuleMap P.Module
  -> ModuleMap (NonEmpty Search)
eligibleModules query decls modules =
  -- TODO: insert search for defining module
  foldMap (directDependants (snd query) modules) (fst query :| findReexportingModules query decls)
