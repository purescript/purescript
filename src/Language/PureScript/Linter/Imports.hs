{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.PureScript.Linter.Imports (findUnusedImports, Name(..), UsedImports()) where

import Prelude ()
import Prelude.Compat

import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.List ((\\), find, intersect)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer.Class
import Control.Monad(unless,when)
import Data.Foldable (forM_)

import Language.PureScript.AST.Declarations
import Language.PureScript.AST.SourcePos
import Language.PureScript.Names as P

import Language.PureScript.Errors
import Language.PureScript.Sugar.Names.Env
import Language.PureScript.Sugar.Names.Imports

import qualified Language.PureScript.Constants as C

-- | Imported name used in some type or expression.
data Name = IdentName (Qualified Ident) | IsProperName (Qualified ProperName) | DctorName (Qualified ProperName)

-- | Map of module name to list of imported names from that module which have been used.
type UsedImports = M.Map ModuleName [Name]

-- |
-- Find and warn on any unused import statements (qualified or unqualified)
-- or references in an explicit import list.
--
findUnusedImports :: forall m. (Applicative m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) => Module -> Env -> UsedImports -> m ()
findUnusedImports (Module _ _ _ mdecls mexports) env usedImps = do
  imps <- findImports mdecls
  forM_ (M.toAscList imps) $ \(mni, decls) -> unless (mni `elem` alwaysUsedModules) $
    forM_ decls $ \(ss, declType, qualifierName) ->
      censor (onErrorMessages $ addModuleLocError ss) $ unless (qnameUsed qualifierName) $
        let names = sugarNames mni ++ M.findWithDefault [] mni usedImps
            usedNames = mapMaybe (matchName (typeForDCtor mni) qualifierName) names
            usedDctors = mapMaybe (matchDctor qualifierName) names
        in case declType of
          Implicit -> when (null usedNames) $ tell $ errorMessage $ UnusedImport mni
          Explicit declrefs -> do
            let idents = mapMaybe runDeclRef declrefs
            let diff = idents \\ usedNames
            case (length diff, length idents) of
              (0, _) -> return ()
              (n, m) | n == m -> tell $ errorMessage $ UnusedImport mni
              _ -> tell $ errorMessage $ UnusedExplicitImport mni diff

            -- If we've not already warned a type is unused, check its data constructors
            forM_ (mapMaybe getTypeRef declrefs) $ \(tn, c) -> do
              let allCtors = dctorsForType mni tn
              when (runProperName tn `elem` usedNames) $ case (c, null $ usedDctors `intersect` allCtors) of
                (Nothing, True) -> tell $ errorMessage $ UnusedDctorImport tn
                (Just (_:_), True) -> tell $ errorMessage $ UnusedDctorImport tn
                (Just ctors, _) ->
                  let ddiff = ctors \\ usedDctors
                  in unless (null ddiff) $ tell $ errorMessage $ UnusedDctorExplicitImport tn ddiff
                _ -> return ()
            return ()

          _ -> return ()
  where
  sugarNames :: ModuleName -> [ Name ]
  sugarNames (ModuleName [ProperName n]) | n == C.prelude = [ IdentName $ Qualified Nothing (Ident C.bind) ]
  sugarNames _ = []

  -- rely on exports being elaborated by this point
  alwaysUsedModules :: [ ModuleName ]
  alwaysUsedModules = ModuleName [ProperName C.prim] : maybe [] (mapMaybe isExport) mexports
    where
      isExport (ModuleRef mn) = Just mn
      isExport _ = Nothing

  qnameUsed :: Maybe ModuleName -> Bool
  qnameUsed (Just qn) = qn `elem` alwaysUsedModules
  qnameUsed Nothing = False

  dtys :: ModuleName -> [((ProperName, [ProperName]), ModuleName)]
  dtys mn = maybe [] exportedTypes $ envModuleExports <$> mn `M.lookup` env

  dctorsForType :: ModuleName -> ProperName -> [ProperName]
  dctorsForType mn tn =
    maybe [] getDctors (find matches $ dtys mn)
    where
      matches ((ty, _),_) = ty == tn
      getDctors ((_,ctors),_) = ctors

  typeForDCtor :: ModuleName -> ProperName -> Maybe ProperName
  typeForDCtor mn pn =
    getTy <$> find matches (dtys mn)
    where
      matches ((_, ctors), _) = pn `elem` ctors
      getTy ((ty, _), _) = ty


matchName :: (ProperName -> Maybe ProperName) -> Maybe ModuleName -> Name -> Maybe String
matchName _ qual (IdentName (Qualified q x)) | q == qual = Just $ showIdent x
matchName _ qual (IsProperName (Qualified q x)) | q == qual = Just $ runProperName x
matchName lookupDc qual (DctorName (Qualified q x)) | q == qual = runProperName <$> lookupDc x
matchName _ _ _ = Nothing

matchDctor :: Maybe ModuleName -> Name -> Maybe ProperName
matchDctor qual (DctorName (Qualified q x)) | q == qual = Just x
matchDctor _ _ = Nothing

runDeclRef :: DeclarationRef -> Maybe String
runDeclRef (PositionedDeclarationRef _ _ ref) = runDeclRef ref
runDeclRef (ValueRef ident) = Just $ showIdent ident
runDeclRef (TypeRef pn _) = Just $ runProperName pn
runDeclRef _ = Nothing

getTypeRef :: DeclarationRef -> Maybe (ProperName, Maybe [ProperName])
getTypeRef (PositionedDeclarationRef _ _ ref) = getTypeRef ref
getTypeRef (TypeRef pn x) = Just (pn, x)
getTypeRef _ = Nothing

addModuleLocError :: Maybe SourceSpan -> ErrorMessage -> ErrorMessage
addModuleLocError sp err =
  case sp of
    Just pos -> withPosition pos err
    _ -> err
