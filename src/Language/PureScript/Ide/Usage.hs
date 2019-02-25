module Language.PureScript.Ide.Usage
  ( findReexportingModules
  , directDependants
  , eligibleModules
  , applySearch
  , findUsages
  ) where

import           Protolude hiding (moduleName)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.PureScript as P
import           Language.PureScript.Ide.State (getAllModules, getFileState)
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util
import           Lens.Micro.Platform (preview)

-- |
-- How we find usages, given an IdeDeclaration and the module it was defined in:
--
-- 1. Find all modules that reexport the given declaration
-- 2. Find all modules that import from those modules, and while traversing the
-- imports build a specification for how the identifier can be found in the
-- module.
-- 3. Apply the collected search specifications and collect the results
findUsages
  :: (MonadIO m, Ide m)
  => IdeDeclaration
  -> P.ModuleName
  -> m (ModuleMap (NonEmpty P.SourceSpan))
findUsages declaration moduleName = do
  ms <- getAllModules Nothing
  asts <- Map.map fst . fsModules <$> getFileState
  let elig = eligibleModules (moduleName, declaration) ms asts
  pure
    $ Map.mapMaybe nonEmpty
    $ Map.mapWithKey (\mn searches ->
        foldMap (\m -> foldMap (applySearch m) searches) (Map.lookup mn asts)) elig

-- | A declaration can either be imported qualified, or unqualified. All the
-- information we need to find usages through a Traversal is thus captured in
-- the `Search` type.
type Search = P.Qualified IdeDeclaration

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
      P.ImportDeclaration _ mn' it qual | mn == mn' -> P.Qualified qual <$> case it of
        P.Implicit -> pure declaration
        P.Explicit refs
          | any (declaration `matchesRef`) refs -> pure declaration
        P.Explicit _ -> []
        P.Hiding refs
          | not (any (declaration `matchesRef`) refs) -> pure declaration
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
  IdeDeclModule m -> case ref of
    P.ModuleRef _ mn -> m == mn
    _ -> False

eligibleModules
  :: (P.ModuleName, IdeDeclaration)
  -> ModuleMap [IdeDeclarationAnn]
  -> ModuleMap P.Module
  -> ModuleMap (NonEmpty Search)
eligibleModules query@(moduleName, declaration) decls modules =
  let
    searchDefiningModule = P.Qualified Nothing declaration :| []
  in
    Map.insert moduleName searchDefiningModule $
      foldMap (directDependants declaration modules) (moduleName :| findReexportingModules query decls)

-- | Finds all usages for a given `Search` throughout a module
applySearch :: P.Module -> Search -> [P.SourceSpan]
applySearch module_ search =
  foldMap findUsageInDeclaration decls
  where
    decls = P.getModuleDeclarations module_
    findUsageInDeclaration =
      let
        (extr, _, _, _, _) = P.everythingWithScope mempty goExpr goBinder mempty mempty
      in
        extr mempty

    goExpr scope expr = case expr of
      P.Var sp i
        | Just ideValue <- preview _IdeDeclValue (P.disqualify search)
        , P.isQualified search
          || not (P.LocalIdent (_ideValueIdent ideValue) `Set.member` scope) ->
          [sp | map P.runIdent i == map identifierFromIdeDeclaration search]
      P.Constructor sp name
        | Just ideDtor <- traverse (preview _IdeDeclDataConstructor) search ->
          [sp | name == map _ideDtorName ideDtor]
      P.Op sp opName
        | Just ideOp <- traverse (preview _IdeDeclValueOperator) search ->
          [sp | opName == map _ideValueOpName ideOp]
      _ -> []

    goBinder _ binder = case binder of
      P.ConstructorBinder sp ctorName _
        | Just ideDtor <- traverse (preview _IdeDeclDataConstructor) search ->
          [sp | ctorName == map _ideDtorName ideDtor]
      P.OpBinder sp opName
        | Just op <- traverse (preview _IdeDeclValueOperator) search ->
          [sp | opName == map _ideValueOpName op]
      _ -> []
