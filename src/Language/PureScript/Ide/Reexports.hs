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
  ) where

import           Protolude

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

-- | Resolves Reexports for a given Module, by looking up the reexported values
-- from the passed in Map
resolveReexports
  :: ModuleMap [IdeDeclarationAnn]
  -- ^ Modules to search for the reexported declarations
  -> (Module, [(P.ModuleName, P.DeclarationRef)])
  -- ^ The module to resolve reexports for, aswell as the references to resolve
  -> ReexportResult Module
resolveReexports modules ((moduleName, decls), refs) =
  ReexportResult (moduleName, decls <> concat resolvedRefs) failedRefs
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
    case findRef (lensSatisfies (_IdeDeclType . ideTypeName) (== tn)) of
      Nothing -> Left ref
      Just d -> Right $ d : case mdtors of
          Nothing ->
            -- If the dataconstructor field inside the TypeRef is Nothing, that
            -- means that all data constructors are exported, so we need to look
            -- those up ourselfes
            findDtors tn
          Just dtors -> mapMaybe lookupDtor dtors
  P.ValueRef i ->
    findWrapped (lensSatisfies (_IdeDeclValue . ideValueIdent) (== i))
  P.ValueOpRef name ->
    findWrapped (lensSatisfies (_IdeDeclValueOperator . ideValueOpName) (== name))
  P.TypeOpRef name ->
    findWrapped (lensSatisfies (_IdeDeclTypeOperator . ideTypeOpName) (== name))
  P.TypeClassRef name ->
    findWrapped (lensSatisfies (_IdeDeclTypeClass . ideTCName) (== name))
  _ ->
    Left ref
  where
    findWrapped = maybe (Left ref) (Right . pure) . findRef
    findRef f = find (f . discardAnn) decls

    lookupDtor name =
      findRef (lensSatisfies (_IdeDeclDataConstructor . ideDtorName) (== name))

    findDtors tn = filter (lensSatisfies
                           (idaDeclaration
                            . _IdeDeclDataConstructor
                            . ideDtorTypeName) (== tn)) decls
