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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Language.PureScript.Ide.Reexports
  ( resolveReexports
  , prettyPrintReexportResult
  , reexportHasFailures
  , ReexportResult(..)
  ) where

import           Protolude

import qualified Data.Map                      as Map
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util
import qualified Language.PureScript as P

-- | Contains the module with resolved reexports, and eventual failures
data ReexportResult a
  = ReexportResult
  { reResolved :: a
  , reFailed :: [(P.ModuleName, P.DeclarationRef)]
  } deriving (Show, Eq, Functor)

-- | Uses the passed formatter to format the resolved module, and adds eventual
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
      <> foldMap (\(mn, ref) -> runModuleNameT mn <> show ref) reFailed

-- | Whether any Refs couldn't be resolved
reexportHasFailures :: ReexportResult a -> Bool
reexportHasFailures = not . null . reFailed

-- | Resolves Reexports for a given Module, by looking up the reexported values
-- from the passed in Map
resolveReexports
  :: Map P.ModuleName [IdeDeclarationAnn]
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
    case findRef (\case IdeType name _ -> name == tn; _ -> False) of
      Nothing -> Left ref
      Just d -> Right $ d : case mdtors of
          Nothing ->
            -- If the dataconstructor field inside the TypeRef is Nothing, that
            -- means that all data constructors are exported, so we need to look
            -- those up ourselfes
            findDtors tn
          Just dtors -> mapMaybe lookupDtor dtors
  P.ValueRef i ->
    findWrapped (\case IdeValue i' _ -> i' == i; _ -> False)
  P.TypeOpRef name ->
    findWrapped (\case IdeTypeOperator n _ _ _ _ -> n == name; _ -> False)
  P.ValueOpRef name ->
    findWrapped (\case IdeValueOperator n _ _ _ _ -> n == name; _ -> False)
  P.TypeClassRef name ->
    findWrapped (\case IdeTypeClass n -> n == name; _ -> False)
  _ ->
    Left ref
  where
    findWrapped = wrapSingle . findRef
    wrapSingle = maybe (Left ref) (Right . pure)
    findRef f = find (f . discardAnn) decls

    lookupDtor name =
      findRef (\case IdeDataConstructor name' _ _ -> name == name'
                     _ -> False)

    findDtors tn = filter (f . discardAnn) decls
      where
        f :: IdeDeclaration -> Bool
        f decl
          | (IdeDataConstructor _ tn' _) <- decl
          , tn == tn' = True
          | otherwise = False
