{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.Linter.Imports (findUnusedImports, Name(..), UsedImports()) where

import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.List ((\\))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer.Class
import Control.Monad(unless,when)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.Foldable (forM_)

import Language.PureScript.AST.Declarations
import Language.PureScript.AST.SourcePos
import Language.PureScript.Names as P

import Language.PureScript.Errors
import Language.PureScript.Sugar.Names.Env
import Language.PureScript.Sugar.Names.Imports

import qualified Language.PureScript.Constants as C

-- | Imported name used in some type or expression.
data Name = IdentName (Qualified Ident) | IsProperName (Qualified ProperName)

-- | Map of module name to list of imported names from that module which have been used.
type UsedImports = M.Map ModuleName [Name]

-- |
-- Find and warn on any unused import statements (qualified or unqualified)
-- or references in an explicit import list.
--
findUnusedImports :: forall m. (Applicative m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) => Module -> Imports -> UsedImports -> m ()
findUnusedImports (Module _ _ _ mdecls _) _ usedImps = do
  imps <- findImports mdecls
  forM_ (M.toAscList imps) $ \(mni, decls) -> unless (mni `elem` autoIncludes) $
    forM_ decls $ \(ss, declType, qualifierName) -> censor (onErrorMessages $ addModuleLocError ss) $
      let usedNames = mapMaybe (matchName qualifierName) $ sugarNames ++ M.findWithDefault [] mni usedImps in
      case declType of
        Implicit -> when (null usedNames) $ tell $ errorMessage $ UnusedImport mni
        Explicit declrefs -> do
          let idents = mapMaybe runDeclRef declrefs
          let diff = idents \\ usedNames
          unless (null diff) $ tell $ errorMessage $ UnusedExplicitImport mni diff
        _ -> return ()
  where
  sugarNames :: [ Name ]
  sugarNames = [ IdentName $ Qualified Nothing (Ident C.bind) ]

  autoIncludes :: [ ModuleName ]
  autoIncludes = [ ModuleName [ProperName C.prim] ]

matchName :: Maybe ModuleName -> Name -> Maybe String
matchName qual (IdentName (Qualified q x)) | q == qual = Just $ runIdent x
matchName qual (IsProperName (Qualified q x)) | q == qual =  Just $ runProperName x
matchName _ _ = Nothing

runDeclRef :: DeclarationRef -> Maybe String
runDeclRef (PositionedDeclarationRef _ _ ref) = runDeclRef ref
runDeclRef (ValueRef ident) = Just $ runIdent ident
runDeclRef (TypeRef pn _) = Just $ runProperName pn
runDeclRef _ = Nothing

addModuleLocError :: Maybe SourceSpan -> ErrorMessage -> ErrorMessage
addModuleLocError sp err =
  case sp of
    Just pos -> withPosition pos err
    _ -> err
