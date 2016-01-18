{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.PureScript.Linter.Imports
  ( lintImports
  , Name(..)
  , UsedImports()
  ) where

import Prelude ()
import Prelude.Compat

import Control.Monad (unless, when)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer.Class

import Data.Foldable (forM_)
import Data.List ((\\), find, intersect, nub)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M

import Language.PureScript.AST.Declarations
import Language.PureScript.AST.SourcePos
import Language.PureScript.Crash
import Language.PureScript.Names as P

import Language.PureScript.Errors
import Language.PureScript.Sugar.Names.Env
import Language.PureScript.Sugar.Names.Imports

import qualified Language.PureScript.Constants as C

-- | Imported name used in some type or expression.
data Name
  = IdentName (Qualified Ident)
  | TyName (Qualified (ProperName 'TypeName))
  | DctorName (Qualified (ProperName 'ConstructorName))
  | TyClassName (Qualified (ProperName 'ClassName))
  deriving (Eq, Show)

getIdentName :: Maybe ModuleName -> Name -> Maybe Ident
getIdentName q (IdentName (Qualified q' name)) | q == q' = Just name
getIdentName _ _ = Nothing

getTypeName :: Maybe ModuleName -> Name -> Maybe (ProperName 'TypeName)
getTypeName q (TyName (Qualified q' name)) | q == q' = Just name
getTypeName _ _ = Nothing

getClassName :: Maybe ModuleName -> Name -> Maybe (ProperName 'ClassName)
getClassName q (TyClassName (Qualified q' name)) | q == q' = Just name
getClassName _ _ = Nothing

-- | Map of module name to list of imported names from that module which have been used.
type UsedImports = M.Map ModuleName [Name]

-- |
-- Find and warn on:
--
-- * Unused import statements (qualified or unqualified)
--
-- * Unused references in an explicit import list
--
-- * Implicit imports of modules
--
-- * Implicit imports into a virtual module (unless the virtual module only has
--   members from one module imported)
--
-- * Imports using `hiding` (this is another form of implicit importing)
--
lintImports
  :: forall m
   . (Applicative m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => Module
  -> Env
  -> UsedImports
  -> m ()
lintImports (Module _ _ mn mdecls mexports) env usedImps = do

  let scope = maybe nullImports (\(_, imps, _) -> imps) (M.lookup mn env)
      usedImps' = foldr (elaborateUsed scope) usedImps exportedModules

  imps <- M.toAscList <$> findImports mdecls

  forM_ imps $ \(mni, decls) ->
    unless (isPrim mni) $ do
      forM_ decls $ \(ss, declType, qualifierName) ->
        censor (onErrorMessages $ addModuleLocError ss) $ do
          let names = nub $ M.findWithDefault [] mni usedImps'
          lintImportDecl env mni qualifierName names declType

  forM_ (M.toAscList (byQual imps)) $ \(mnq, entries) -> do
    let mnis = nub $ map (\(_, _, mni) -> mni) entries
    unless (length mnis == 1) $ do
      let implicits = filter (\(_, declType, _) -> not $ isExplicit declType) entries
      forM_ implicits $ \(ss, _, mni) ->
        censor (onErrorMessages $ addModuleLocError ss) $ do
          let names = nub $ M.findWithDefault [] mni usedImps'
              usedRefs = findUsedRefs env mni (Just mnq) names
          unless (null usedRefs) $
            tell $ errorMessage $ ImplicitQualifiedImport mni mnq usedRefs

  return ()

  where

  -- Checks whether a module is the Prim module - used to suppress any checks
  -- made, as Prim is always implicitly imported.
  isPrim :: ModuleName -> Bool
  isPrim = (== ModuleName [ProperName C.prim])

  -- Creates a map of virtual modules mapped to all the declarations that
  -- import to that module, with the corresponding source span, import type,
  -- and module being imported
  byQual
    :: [(ModuleName, [(Maybe SourceSpan, ImportDeclarationType, Maybe ModuleName)])]
    -> M.Map ModuleName [(Maybe SourceSpan, ImportDeclarationType, ModuleName)]
  byQual = foldr goImp M.empty
    where
    goImp (mni, xs) acc = foldr (goDecl mni) acc xs
    goDecl mni (ss, declType, Just qmn) acc =
      let entry = (ss, declType, mni)
      in M.alter (Just . maybe [entry] (entry :)) qmn acc
    goDecl _ _ acc = acc

  -- The list of modules that are being re-exported by the current module. Any
  -- module that appears in this list is always considered to be used.
  exportedModules :: [ModuleName]
  exportedModules = nub $ maybe [] (mapMaybe extractModule) mexports
    where
    extractModule (PositionedDeclarationRef _ _ r) = extractModule r
    extractModule (ModuleRef mne) = Just mne
    extractModule _ = Nothing

  -- Elaborates the UsedImports to include values from modules that are being
  -- re-exported. This ensures explicit export hints are printed for modules
  -- that are implicitly exported and then re-exported.
  elaborateUsed :: Imports -> ModuleName -> UsedImports -> UsedImports
  elaborateUsed scope mne used =
    let classes = extractByQual mne (importedTypeClasses scope) TyClassName
        types = extractByQual mne (importedTypes scope) TyName
        dctors = extractByQual mne (importedDataConstructors scope) DctorName
        values = extractByQual mne (importedValues scope) IdentName
    in foldr go used (classes ++ types ++ dctors ++ values)
    where
    go :: (ModuleName, Name) -> UsedImports -> UsedImports
    go (q, name) acc = M.alter (Just . maybe [name] (name :)) q acc

  extractByQual
    :: (Eq a)
    => ModuleName
    -> M.Map (Qualified a) [(Qualified a, ModuleName)]
    -> (Qualified a -> Name)
    -> [(ModuleName, Name)]
  extractByQual k m toName = mapMaybe go (M.toList m)
    where
    go (q@(Qualified mnq _), is) | isUnqualified q || isQualifiedWith k q =
      case fst (head is) of
        Qualified (Just mn') name -> Just (mn', toName $ Qualified mnq name)
        _ -> internalError "unqualified name in extractByQual"
    go _ = Nothing

lintImportDecl
  :: forall m
   . (Applicative m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => Env
  -> ModuleName
  -> Maybe ModuleName
  -> [Name]
  -> ImportDeclarationType
  -> m ()
lintImportDecl env mni qualifierName names declType =
  case declType of
    Implicit -> case qualifierName of
      Nothing -> checkImplicit ImplicitImport
      Just q ->
        let usedModuleNames = mapMaybe extractQualName names
        in unless (q `elem` usedModuleNames) unused
    Hiding _ -> checkImplicit HidingImport
    Explicit [] -> unused
    Explicit declrefs -> checkExplicit declrefs

  where

  checkImplicit
    :: (ModuleName -> [DeclarationRef] -> SimpleErrorMessage)
    -> m ()
  checkImplicit warning =
    if null allRefs
    then unused
    else tell $ errorMessage $ warning mni allRefs

  checkExplicit
    :: [DeclarationRef]
    -> m ()
  checkExplicit declrefs = do
    let idents = nub (mapMaybe runDeclRef declrefs)
        dctors = mapMaybe (matchDctor qualifierName) names
        usedNames = mapMaybe (matchName (typeForDCtor mni) qualifierName) names
        diff = idents \\ usedNames
    case (length diff, length idents) of
      (0, _) -> return ()
      (n, m) | n == m -> unused
      _ -> tell $ errorMessage $ UnusedExplicitImport mni diff qualifierName allRefs

    -- If we've not already warned a type is unused, check its data constructors
    forM_ (mapMaybe getTypeRef declrefs) $ \(tn, c) -> do
      let allCtors = dctorsForType mni tn
      when (runProperName tn `elem` usedNames) $ case (c, dctors `intersect` allCtors) of
        (_, []) | c /= Just [] ->
          tell $ errorMessage $ UnusedDctorImport tn
        (Just ctors, dctors') ->
          let ddiff = ctors \\ dctors'
          in unless (null ddiff) $ tell $ errorMessage $ UnusedDctorExplicitImport tn ddiff
        _ -> return ()
    return ()

  unused :: m ()
  unused = tell $ errorMessage $ UnusedImport mni

  allRefs :: [DeclarationRef]
  allRefs = findUsedRefs env mni qualifierName names

  dtys
    :: ModuleName
    -> [((ProperName 'TypeName, [ProperName 'ConstructorName]), ModuleName)]
  dtys mn = maybe [] exportedTypes $ envModuleExports <$> mn `M.lookup` env

  dctorsForType
    :: ModuleName
    -> ProperName 'TypeName
    -> [ProperName 'ConstructorName]
  dctorsForType mn tn =
    maybe [] getDctors (find matches $ dtys mn)
    where
      matches ((ty, _),_) = ty == tn
      getDctors ((_,ctors),_) = ctors

  typeForDCtor
    :: ModuleName
    -> ProperName 'ConstructorName
    -> Maybe (ProperName 'TypeName)
  typeForDCtor mn pn =
    getTy <$> find matches (dtys mn)
    where
      matches ((_, ctors), _) = pn `elem` ctors
      getTy ((ty, _), _) = ty

findUsedRefs :: Env -> ModuleName -> Maybe ModuleName -> [Name] -> [DeclarationRef]
findUsedRefs env mni qualifierName names =
  let
    classRefs = TypeClassRef <$> mapMaybe (getClassName qualifierName) names
    valueRefs = ValueRef <$> mapMaybe (getIdentName qualifierName) names
    types = mapMaybe (getTypeName qualifierName) names
    dctors = mapMaybe (matchDctor qualifierName) names
    typesWithDctors = reconstructTypeRefs dctors
    typesWithoutDctors = filter (`M.notMember` typesWithDctors) types
    typesRefs
      = map (flip TypeRef (Just [])) typesWithoutDctors
      ++ map (\(ty, ds) -> TypeRef ty (Just ds)) (M.toList typesWithDctors)
  in classRefs ++ typesRefs ++ valueRefs

  where

  reconstructTypeRefs
    :: [ProperName 'ConstructorName]
    -> M.Map (ProperName 'TypeName) [ProperName 'ConstructorName]
  reconstructTypeRefs = foldr accumDctors M.empty
    where
    accumDctors dctor = M.alter (Just . maybe [dctor] (dctor :)) (findTypeForDctor mni dctor)

  findTypeForDctor
    :: ModuleName
    -> ProperName 'ConstructorName
    -> ProperName 'TypeName
  findTypeForDctor mn dctor =
    case mn `M.lookup` env of
      Just (_, _, exps) ->
        case find (elem dctor . snd . fst) (exportedTypes exps) of
          Just ((ty, _), _) -> ty
          Nothing -> internalError $ "missing type for data constructor " ++ runProperName dctor ++ " in findTypeForDctor"
      Nothing -> internalError $ "missing module " ++ runModuleName mn  ++ " in findTypeForDctor"

matchName
  :: (ProperName 'ConstructorName -> Maybe (ProperName 'TypeName))
  -> Maybe ModuleName
  -> Name
  -> Maybe String
matchName _ qual (IdentName (Qualified q x)) | q == qual = Just $ showIdent x
matchName _ qual (TyName (Qualified q x)) | q == qual = Just $ runProperName x
matchName _ qual (TyClassName (Qualified q x)) | q == qual = Just $ runProperName x
matchName lookupDc qual (DctorName (Qualified q x)) | q == qual = runProperName <$> lookupDc x
matchName _ _ _ = Nothing

extractQualName :: Name -> Maybe ModuleName
extractQualName (IdentName (Qualified q _)) = q
extractQualName (TyName (Qualified q _)) = q
extractQualName (TyClassName (Qualified q _)) = q
extractQualName (DctorName (Qualified q _)) = q

matchDctor :: Maybe ModuleName -> Name -> Maybe (ProperName 'ConstructorName)
matchDctor qual (DctorName (Qualified q x)) | q == qual = Just x
matchDctor _ _ = Nothing

runDeclRef :: DeclarationRef -> Maybe String
runDeclRef (PositionedDeclarationRef _ _ ref) = runDeclRef ref
runDeclRef (ValueRef ident) = Just $ showIdent ident
runDeclRef (TypeRef pn _) = Just $ runProperName pn
runDeclRef (TypeClassRef pn) = Just $ runProperName pn
runDeclRef _ = Nothing

getTypeRef
  :: DeclarationRef
  -> Maybe (ProperName 'TypeName, Maybe [ProperName 'ConstructorName])
getTypeRef (PositionedDeclarationRef _ _ ref) = getTypeRef ref
getTypeRef (TypeRef pn x) = Just (pn, x)
getTypeRef _ = Nothing

addModuleLocError :: Maybe SourceSpan -> ErrorMessage -> ErrorMessage
addModuleLocError sp err =
  case sp of
    Just pos -> withPosition pos err
    _ -> err
