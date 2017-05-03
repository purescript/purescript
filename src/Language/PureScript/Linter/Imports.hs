module Language.PureScript.Linter.Imports
  ( lintImports
  , Name(..)
  , UsedImports()
  ) where

import Prelude.Compat
import Protolude (ordNub)

import Control.Monad (join, unless, foldM, (<=<))
import Control.Monad.Writer.Class

import Data.Function (on)
import Data.Foldable (for_)
import Data.List (find, intersect, groupBy, sortBy, (\\))
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Monoid (Sum(..))
import Data.Traversable (forM)
import qualified Data.Text as T
import qualified Data.Map as M

import Language.PureScript.AST.Declarations
import Language.PureScript.AST.SourcePos
import Language.PureScript.Crash
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.Sugar.Names.Common (warnDuplicateRefs)
import Language.PureScript.Sugar.Names.Env
import Language.PureScript.Sugar.Names.Imports
import qualified Language.PureScript.Constants as C

-- |
-- Map of module name to list of imported names from that module which have
-- been used.
--
type UsedImports = M.Map ModuleName [Qualified Name]

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
   . MonadWriter MultipleErrors m
  => Module
  -> Env
  -> UsedImports
  -> m ()
lintImports (Module _ _ _ _ Nothing) _ _ =
  internalError "lintImports needs desugared exports"
lintImports (Module ss _ mn mdecls (Just mexports)) env usedImps = do

  -- TODO: this needs some work to be easier to understand

  let scope = maybe primImports (\(_, imps', _) -> imps') (M.lookup mn env)
      usedImps' = foldr (elaborateUsed scope) usedImps exportedModules
      numOpenImports = getSum $ foldMap (Sum . countOpenImports) mdecls
      allowImplicit = numOpenImports == 1
      imports = M.toAscList (findImports mdecls)

  for_ imports $ \(mni, decls) ->
    unless (isPrim mni) $
      for_ decls $ \(ss', declType, qualifierName) ->
        maybe id warnWithPosition ss' $ do
          let names = ordNub $ M.findWithDefault [] mni usedImps'
          lintImportDecl env mni qualifierName names declType allowImplicit

  for_ (M.toAscList (byQual imports)) $ \(mnq, entries) -> do
    let mnis = ordNub $ map (\(_, _, mni) -> mni) entries
    unless (length mnis == 1) $ do
      let implicits = filter (\(_, declType, _) -> not $ isExplicit declType) entries
      for_ implicits $ \(ss', _, mni) ->
        maybe id warnWithPosition ss' $ do
          let names = ordNub $ M.findWithDefault [] mni usedImps'
              usedRefs = findUsedRefs env mni (Just mnq) names
          unless (null usedRefs) $
            tell $ errorMessage $ ImplicitQualifiedImport mni mnq usedRefs

  for_ imports $ \(mnq, imps) -> do

    warned <- foldM (checkDuplicateImports mnq) [] (selfCartesianSubset imps)

    let unwarned = imps \\ warned
        duplicates
          = join
          . map tail
          . filter ((> 1) . length)
          . groupBy ((==) `on` defQual)
          . sortBy (compare `on` defQual)
          $ unwarned

    for_ duplicates $ \(pos, _, _) ->
      maybe id warnWithPosition pos $
        tell $ errorMessage $ DuplicateSelectiveImport mnq

    for_ (imps \\ (warned ++ duplicates)) $ \(pos, typ, _) ->
      warnDuplicateRefs (fromMaybe ss pos) DuplicateImportRef $ case typ of
        Explicit refs -> refs
        Hiding refs -> refs
        _ -> []

  where

  defQual :: ImportDef -> Maybe ModuleName
  defQual (_, _, q) = q

  selfCartesianSubset :: [a] -> [(a, a)]
  selfCartesianSubset (x : xs) = [(x, y) | y <- xs] ++ selfCartesianSubset xs
  selfCartesianSubset [] = []

  countOpenImports :: Declaration -> Int
  countOpenImports (PositionedDeclaration _ _ d) = countOpenImports d
  countOpenImports (ImportDeclaration mn' Implicit Nothing)
    | not (isPrim mn' || mn == mn') = 1
  countOpenImports (ImportDeclaration mn' (Hiding _) Nothing)
    | not (isPrim mn' || mn == mn') = 1
  countOpenImports _ = 0

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
    goDecl mni (ss', declType, Just qmn) acc =
      let entry = (ss', declType, mni)
      in M.alter (Just . maybe [entry] (entry :)) qmn acc
    goDecl _ _ acc = acc

  -- The list of modules that are being re-exported by the current module. Any
  -- module that appears in this list is always considered to be used.
  exportedModules :: [ModuleName]
  exportedModules = ordNub $ mapMaybe extractModule mexports
    where
    extractModule (PositionedDeclarationRef _ _ r) = extractModule r
    extractModule (ModuleRef mne) = Just mne
    extractModule _ = Nothing

  -- Elaborates the UsedImports to include values from modules that are being
  -- re-exported. This ensures explicit export hints are printed for modules
  -- that are implicitly exported and then re-exported.
  elaborateUsed :: Imports -> ModuleName -> UsedImports -> UsedImports
  elaborateUsed scope mne used =
    foldr go used
      $ extractByQual mne (importedTypeClasses scope) TyClassName
      ++ extractByQual mne (importedTypeOps scope) TyOpName
      ++ extractByQual mne (importedTypes scope) TyName
      ++ extractByQual mne (importedDataConstructors scope) DctorName
      ++ extractByQual mne (importedValues scope) IdentName
      ++ extractByQual mne (importedValueOps scope) ValOpName
    where
    go :: (ModuleName, Qualified Name) -> UsedImports -> UsedImports
    go (q, name) = M.alter (Just . maybe [name] (name :)) q

  extractByQual
    :: ModuleName
    -> M.Map (Qualified a) [ImportRecord a]
    -> (a -> Name)
    -> [(ModuleName, Qualified Name)]
  extractByQual k m toName = mapMaybe go (M.toList m)
    where
    go (q@(Qualified mnq _), is)
      | isUnqualified q =
          case find (isQualifiedWith k) (map importName is) of
            Just (Qualified _ name) -> Just (k, Qualified mnq (toName name))
            _ -> Nothing
      | isQualifiedWith k q =
          case importName (head is) of
            Qualified (Just mn') name -> Just (mn', Qualified mnq (toName name))
            _ -> internalError "unqualified name in extractByQual"
    go _ = Nothing

lintImportDecl
  :: forall m
   . MonadWriter MultipleErrors m
  => Env
  -> ModuleName
  -> Maybe ModuleName
  -> [Qualified Name]
  -> ImportDeclarationType
  -> Bool
  -> m Bool
lintImportDecl env mni qualifierName names declType allowImplicit =
  case declType of
    Implicit -> case qualifierName of
      Nothing ->
        if null allRefs
        then unused
        else unless' allowImplicit (checkImplicit ImplicitImport)
      Just q -> unless' (q `elem` mapMaybe getQual names) unused
    Hiding _ -> unless' allowImplicit (checkImplicit HidingImport)
    Explicit [] -> unused
    Explicit declrefs -> checkExplicit declrefs

  where

  checkImplicit
    :: (ModuleName -> [DeclarationRef] -> SimpleErrorMessage)
    -> m Bool
  checkImplicit warning =
    if null allRefs
    then unused
    else warn (warning mni (map simplifyTypeRef allRefs))
    where
    -- Replace explicit type refs with data constructor lists from listing the
    -- used constructors explicity `T(X, Y, [...])` to `T(..)` for suggestion
    -- message.
    simplifyTypeRef :: DeclarationRef -> DeclarationRef
    simplifyTypeRef (TypeRef name (Just dctors))
      | not (null dctors) = TypeRef name Nothing
    simplifyTypeRef other = other

  checkExplicit
    :: [DeclarationRef]
    -> m Bool
  checkExplicit declrefs = do
    let idents = ordNub (mapMaybe runDeclRef declrefs)
        dctors = mapMaybe (getDctorName <=< disqualifyFor qualifierName) names
        usedNames = mapMaybe (matchName (typeForDCtor mni) <=< disqualifyFor qualifierName) names
        diff = idents \\ usedNames

    didWarn <- case (length diff, length idents) of
      (0, _) -> return False
      (n, m) | n == m -> unused
      _ -> warn (UnusedExplicitImport mni diff qualifierName allRefs)

    didWarn' <- forM (mapMaybe getTypeRef declrefs) $ \(tn, c) -> do
      let allCtors = dctorsForType mni tn
      -- If we've not already warned a type is unused, check its data constructors
      unless' (TyName tn `notElem` usedNames) $
        case (c, dctors `intersect` allCtors) of
          (_, []) | c /= Just [] -> warn (UnusedDctorImport mni tn qualifierName allRefs)
          (Just ctors, dctors') ->
            let ddiff = ctors \\ dctors'
            in unless' (null ddiff) $ warn $ UnusedDctorExplicitImport mni tn ddiff qualifierName allRefs
          _ -> return False

    return (didWarn || or didWarn')

  unused :: m Bool
  unused = warn (UnusedImport mni)

  warn :: SimpleErrorMessage -> m Bool
  warn err = tell (errorMessage err) >> return True

  -- Unless the boolean is true, run the action. Return false when the action is
  -- not run, otherwise return whatever the action does.
  --
  -- The return value is intended for cases where we want to track whether some
  -- work was done, as there may be further conditions in the action that mean
  -- it ends up doing nothing.
  unless' :: Bool -> m Bool -> m Bool
  unless' False m = m
  unless' True _ = return False

  allRefs :: [DeclarationRef]
  allRefs = findUsedRefs env mni qualifierName names

  dtys
    :: ModuleName
    -> M.Map (ProperName 'TypeName) ([ProperName 'ConstructorName], ModuleName)
  dtys mn = maybe M.empty exportedTypes $ envModuleExports <$> mn `M.lookup` env

  dctorsForType
    :: ModuleName
    -> ProperName 'TypeName
    -> [ProperName 'ConstructorName]
  dctorsForType mn tn = maybe [] fst $ tn `M.lookup` dtys mn

  typeForDCtor
    :: ModuleName
    -> ProperName 'ConstructorName
    -> Maybe (ProperName 'TypeName)
  typeForDCtor mn pn = fst <$> find (elem pn . fst . snd) (M.toList (dtys mn))

findUsedRefs
  :: Env
  -> ModuleName
  -> Maybe ModuleName
  -> [Qualified Name]
  -> [DeclarationRef]
findUsedRefs env mni qn names =
  let
    classRefs = TypeClassRef <$> mapMaybe (getClassName <=< disqualifyFor qn) names
    valueRefs = ValueRef <$> mapMaybe (getIdentName <=< disqualifyFor qn) names
    valueOpRefs = ValueOpRef <$> mapMaybe (getValOpName <=< disqualifyFor qn) names
    typeOpRefs = TypeOpRef <$> mapMaybe (getTypeOpName <=< disqualifyFor qn) names
    types = mapMaybe (getTypeName <=< disqualifyFor qn) names
    dctors = mapMaybe (getDctorName <=< disqualifyFor qn) names
    typesWithDctors = reconstructTypeRefs dctors
    typesWithoutDctors = filter (`M.notMember` typesWithDctors) types
    typesRefs
      = map (flip TypeRef (Just [])) typesWithoutDctors
      ++ map (\(ty, ds) -> TypeRef ty (Just ds)) (M.toList typesWithDctors)
  in sortBy compDecRef $ classRefs ++ typeOpRefs ++ typesRefs ++ valueRefs ++ valueOpRefs

  where

  reconstructTypeRefs
    :: [ProperName 'ConstructorName]
    -> M.Map (ProperName 'TypeName) [ProperName 'ConstructorName]
  reconstructTypeRefs = foldr accumDctors M.empty
    where
    accumDctors dctor =
      M.alter (Just . maybe [dctor] (dctor :)) (findTypeForDctor mni dctor)

  findTypeForDctor
    :: ModuleName
    -> ProperName 'ConstructorName
    -> ProperName 'TypeName
  findTypeForDctor mn dctor =
    case mn `M.lookup` env of
      Just (_, _, exps) ->
        case find (elem dctor . fst . snd) (M.toList (exportedTypes exps)) of
          Just (ty, _) -> ty
          Nothing -> internalError $ "missing type for data constructor " ++ T.unpack (runProperName dctor) ++ " in findTypeForDctor"
      Nothing -> internalError $ "missing module " ++ T.unpack (runModuleName mn)  ++ " in findTypeForDctor"

matchName
  :: (ProperName 'ConstructorName -> Maybe (ProperName 'TypeName))
  -> Name
  -> Maybe Name
matchName lookupDc (DctorName x) = TyName <$> lookupDc x
matchName _ ModName{} = Nothing
matchName _ name = Just name

runDeclRef :: DeclarationRef -> Maybe Name
runDeclRef (PositionedDeclarationRef _ _ ref) = runDeclRef ref
runDeclRef (ValueRef ident) = Just $ IdentName ident
runDeclRef (ValueOpRef op) = Just $ ValOpName op
runDeclRef (TypeRef pn _) = Just $ TyName pn
runDeclRef (TypeOpRef op) = Just $ TyOpName op
runDeclRef (TypeClassRef pn) = Just $ TyClassName pn
runDeclRef _ = Nothing

checkDuplicateImports
  :: MonadWriter MultipleErrors m
  => ModuleName
  -> [ImportDef]
  -> (ImportDef, ImportDef)
  -> m [ImportDef]
checkDuplicateImports mn xs ((_, t1, q1), (pos, t2, q2)) =
  if t1 == t2 && q1 == q2
  then do
    maybe id warnWithPosition pos $
      tell $ errorMessage $ DuplicateImport mn t2 q2
    return $ (pos, t2, q2) : xs
  else return xs
