-----------------------------------------------------------------------------
--
-- Module      : Language.PureScript.Ide.Reexports
-- Description : Resolves reexports for psc-ide
-- Copyright   : Christoph Hegemann 2016
--               Brian Sermons 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- Resolves reexports for psc-ide
-----------------------------------------------------------------------------

module Language.PureScript.Ide.Reexports
  ( resolveReexports
  , prettyPrintReexportResult
  , reexportHasFailures
  , ReexportResult(..)
  -- for tests
  , resolveReexports'
  ) where

import           Protolude

import           Control.Lens                  hiding ((&))
import qualified Data.Map                      as Map
import qualified Language.PureScript           as P
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util

-- | Contains the module with resolved reexports, and possible failures
data ReexportResult a
  = ReexportResult
  { reResolved :: a
  , reFailed   :: [(P.ModuleName, P.DeclarationRef)]
  } deriving (Show, Eq, Functor)

-- | Uses the passed formatter to format the resolved module, and adds possible
-- failures
prettyPrintReexportResult
  :: (a -> Text)
  -- ^ Formatter for the resolved result
  -> ReexportResult a
  -- ^ The Result to be pretty printed
  -> Text
prettyPrintReexportResult f ReexportResult{..}
  | null reFailed =
      "Successfully resolved reexports for " <> f reResolved
  | otherwise =
      "Failed to resolve reexports for "
      <> f reResolved
      <> foldMap (\(mn, ref) -> P.runModuleName mn <> show ref) reFailed

-- | Whether any Refs couldn't be resolved
reexportHasFailures :: ReexportResult a -> Bool
reexportHasFailures = not . null . reFailed

-- | Resolves Reexports for the given Modules, by looking up the reexported
-- values from the passed in DeclarationRefs
resolveReexports
  :: ModuleMap [(P.ModuleName, P.DeclarationRef)]
  -- ^ the references to resolve
  -> ModuleMap [IdeDeclarationAnn]
  -- ^ Modules to search for the reexported declarations
  -> ModuleMap (ReexportResult [IdeDeclarationAnn])
resolveReexports reexportRefs modules =
  Map.mapWithKey (\moduleName decls ->
                    maybe (ReexportResult decls [])
                      (resolveReexports' modules decls)
                      (Map.lookup moduleName reexportRefs)) modules

resolveReexports'
  :: ModuleMap [IdeDeclarationAnn]
  -> [IdeDeclarationAnn]
  -> [(P.ModuleName, P.DeclarationRef)]
  -> ReexportResult [IdeDeclarationAnn]
resolveReexports' modules decls refs =
  ReexportResult (decls <> concat resolvedRefs) failedRefs
  where
    (failedRefs, resolvedRefs) = partitionEithers (resolveRef' <$> refs)
    resolveRef' x@(mn, r) = case Map.lookup mn modules of
      Nothing -> Left x
      Just decls' -> first (mn,) (resolveRef decls' r)

resolveRef
  :: [IdeDeclarationAnn]
  -> P.DeclarationRef
  -> Either P.DeclarationRef [IdeDeclarationAnn]
resolveRef decls ref = case ref of
  P.TypeRef tn mdtors ->
    case findRef (anyOf (_IdeDeclType . ideTypeName) (== tn)) of
      Nothing -> Left ref
      Just d -> Right $ d : case mdtors of
          Nothing ->
            -- If the dataconstructor field inside the TypeRef is Nothing, that
            -- means that all data constructors are exported, so we need to look
            -- those up ourselfes
            findDtors tn
          Just dtors -> mapMaybe lookupDtor dtors
  P.ValueRef i ->
    findWrapped (anyOf (_IdeDeclValue . ideValueIdent) (== i))
  P.ValueOpRef name ->
    findWrapped (anyOf (_IdeDeclValueOperator . ideValueOpName) (== name))
  P.TypeOpRef name ->
    findWrapped (anyOf (_IdeDeclTypeOperator . ideTypeOpName) (== name))
  P.TypeClassRef name ->
    findWrapped (anyOf (_IdeDeclTypeClass . ideTCName) (== name))
  _ ->
    Left ref
  where
    findWrapped = maybe (Left ref) (Right . pure) . findRef
    findRef f = find (f . discardAnn) decls

    lookupDtor name =
      findRef (anyOf (_IdeDeclDataConstructor . ideDtorName) (== name))

    findDtors tn = filter (anyOf
                           (idaDeclaration
                            . _IdeDeclDataConstructor
                            . ideDtorTypeName) (== tn)) decls
