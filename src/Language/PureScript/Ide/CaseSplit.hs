-----------------------------------------------------------------------------
--
-- Module      : Language.PureScript.Ide.CaseSplit
-- Description : Casesplitting and adding function clauses
-- Copyright   : Christoph Hegemann 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- Casesplitting and adding function clauses
-----------------------------------------------------------------------------

module Language.PureScript.Ide.CaseSplit
       ( WildcardAnnotations()
       , explicitAnnotations
       , noAnnotations
       , makePattern
       , addClause
       , caseSplit
       ) where

import           Protolude                     hiding (Constructor)

import qualified Data.List.NonEmpty            as NE
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Language.PureScript           as P
import qualified Language.PureScript.CST       as CST

import           Language.PureScript.Externs
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.State
import           Language.PureScript.Ide.Types

type Constructor = (P.ProperName 'P.ConstructorName, [P.SourceType])

newtype WildcardAnnotations = WildcardAnnotations Bool

explicitAnnotations :: WildcardAnnotations
explicitAnnotations = WildcardAnnotations True

noAnnotations :: WildcardAnnotations
noAnnotations = WildcardAnnotations False

type DataType = ([(Text, Maybe P.SourceKind)], [(P.ProperName 'P.ConstructorName, [P.SourceType])])

caseSplit
  :: (Ide m, MonadError IdeError m)
  => Text
  -> m [Constructor]
caseSplit q = do
  type' <- parseType' q
  (tc, args) <- splitTypeConstructor type'
  (typeVars, ctors) <- findTypeDeclaration tc
  let applyTypeVars = P.everywhereOnTypes (P.replaceAllTypeVars (zip (map fst typeVars) args))
  let appliedCtors = map (second (map applyTypeVars)) ctors
  pure appliedCtors

findTypeDeclaration
  :: (Ide m, MonadError IdeError m)
  => P.ProperName 'P.TypeName
  -> m DataType
findTypeDeclaration q = do
  efs <- getExternFiles
  efs' <- maybe efs (flip (uncurry M.insert) efs) <$> cachedRebuild
  let m = getFirst $ foldMap (findTypeDeclaration' q) efs'
  case m of
    Just mn -> pure mn
    Nothing -> throwError (GeneralError "Not Found")

findTypeDeclaration'
  :: P.ProperName 'P.TypeName
  -> ExternsFile
  -> First DataType
findTypeDeclaration' t ExternsFile{..} =
  First $ head $ mapMaybe (\case
            EDType tn _ (P.DataType typeVars ctors)
              | tn == t -> Just (typeVars, ctors)
            _ -> Nothing) efDeclarations

splitTypeConstructor :: (MonadError IdeError m) =>
                        P.Type a -> m (P.ProperName 'P.TypeName, [P.Type a])
splitTypeConstructor = go []
  where
    go acc (P.TypeApp _ ty arg) = go (arg : acc) ty
    go acc (P.TypeConstructor _ tc) = pure (P.disqualify tc, acc)
    go _ _ = throwError (GeneralError "Failed to read TypeConstructor")

prettyCtor :: WildcardAnnotations -> Constructor -> Text
prettyCtor _ (ctorName, []) = P.runProperName ctorName
prettyCtor wsa (ctorName, ctorArgs) =
  "("<> P.runProperName ctorName <> " "
  <> T.unwords (map (prettyPrintWildcard wsa) ctorArgs) <>")"

prettyPrintWildcard :: WildcardAnnotations -> P.Type a -> Text
prettyPrintWildcard (WildcardAnnotations True) = prettyWildcard
prettyPrintWildcard (WildcardAnnotations False) = const "_"

prettyWildcard :: P.Type a -> Text
prettyWildcard t = "( _ :: " <> T.strip (T.pack (P.prettyPrintTypeAtom maxBound t)) <> ")"

-- | Constructs Patterns to insert into a sourcefile
makePattern :: Text -- ^ Current line
            -> Int -- ^ Begin of the split
            -> Int -- ^ End of the split
            -> WildcardAnnotations -- ^ Whether to explicitly type the splits
            -> [Constructor] -- ^ Constructors to split
            -> [Text]
makePattern t x y wsa = makePattern' (T.take x t) (T.drop y t)
  where
    makePattern' lhs rhs = map (\ctor -> lhs <> prettyCtor wsa ctor <> rhs)

addClause :: (MonadError IdeError m) => Text -> WildcardAnnotations -> m [Text]
addClause s wca = do
  (fName, fType) <- parseTypeDeclaration' s
  let args = splitFunctionType fType
      template = P.runIdent fName <> " " <>
        T.unwords (map (prettyPrintWildcard wca) args) <>
        " = ?" <> (T.strip . P.runIdent $ fName)
  pure [s, template]

parseType' :: (MonadError IdeError m) =>
              Text -> m P.SourceType
parseType' s =
  case CST.runTokenParser CST.parseType $ CST.lex s of
    Right type' -> pure $ CST.convertType "<purs-ide>" type'
    Left err ->
      throwError (GeneralError ("Parsing the splittype failed with:"
                                <> show err))

parseTypeDeclaration' :: (MonadError IdeError m) => Text -> m (P.Ident, P.SourceType)
parseTypeDeclaration' s =
  let x = fmap (CST.convertDeclaration "<purs-ide>")
        $ CST.runTokenParser CST.parseDecl
        $ CST.lex s
  in
    case x of
      Right [P.TypeDeclaration td] -> pure (P.unwrapTypeDeclaration td)
      Right _ -> throwError (GeneralError "Found a non-type-declaration")
      Left errs ->
        throwError (GeneralError ("Parsing the type signature failed with: "
                                   <> toS (CST.prettyPrintErrorMessage $ NE.head errs)))

splitFunctionType :: P.Type a -> [P.Type a]
splitFunctionType t = fromMaybe [] arguments
  where
    arguments = initMay splitted
    splitted = splitType' t
    splitType' (P.ForAll _ _ _ t' _) = splitType' t'
    splitType' (P.ConstrainedType _ _ t') = splitType' t'
    splitType' (P.TypeApp _ (P.TypeApp _ t' lhs) rhs)
          | P.eqType t' P.tyFunction = lhs : splitType' rhs
    splitType' t' = [t']
