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

{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.Ide.CaseSplit
       ( WildcardAnnotations()
       , explicitAnnotations
       , noAnnotations
       , makePattern
       , addClause
       , caseSplit
       ) where

import           Protolude                     hiding (Constructor)

import qualified Data.Text                     as T
import qualified Language.PureScript           as P

import           Language.PureScript.Externs
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.State
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util

import           Text.Parsec                   as Parsec
import qualified Text.PrettyPrint.Boxes        as Box

type Constructor = (P.ProperName 'P.ConstructorName, [P.Type])

newtype WildcardAnnotations = WildcardAnnotations Bool

explicitAnnotations :: WildcardAnnotations
explicitAnnotations = WildcardAnnotations True

noAnnotations :: WildcardAnnotations
noAnnotations = WildcardAnnotations False

caseSplit :: (Ide m, MonadError PscIdeError m) =>
             Text -> m [Constructor]
caseSplit q = do
  type' <- parseType' q
  (tc, args) <- splitTypeConstructor type'
  (EDType _ _ (P.DataType typeVars ctors)) <- findTypeDeclaration tc
  let applyTypeVars = P.everywhereOnTypes (P.replaceAllTypeVars (zip (map fst typeVars) args))
  let appliedCtors = map (second (map applyTypeVars)) ctors
  pure appliedCtors

findTypeDeclaration :: (Ide m, MonadError PscIdeError m) =>
                         P.ProperName 'P.TypeName -> m ExternsDeclaration
findTypeDeclaration q = do
  efs <- getExternFiles
  let m = getFirst $ foldMap (findTypeDeclaration' q) efs
  case m of
    Just mn -> pure mn
    Nothing -> throwError (GeneralError "Not Found")

findTypeDeclaration' ::
  P.ProperName 'P.TypeName
  -> ExternsFile
  -> First ExternsDeclaration
findTypeDeclaration' t ExternsFile{..} =
  First $ find (\case
            EDType tn _ _ -> tn == t
            _ -> False) efDeclarations

splitTypeConstructor :: (MonadError PscIdeError m) =>
                        P.Type -> m (P.ProperName 'P.TypeName, [P.Type])
splitTypeConstructor = go []
  where
    go acc (P.TypeApp ty arg) = go (arg : acc) ty
    go acc (P.TypeConstructor tc) = pure (P.disqualify tc, acc)
    go _ _ = throwError (GeneralError "Failed to read TypeConstructor")

prettyCtor :: WildcardAnnotations -> Constructor -> Text
prettyCtor _ (ctorName, []) = runProperNameT ctorName
prettyCtor wsa (ctorName, ctorArgs) =
  "("<> runProperNameT ctorName <> " "
  <> T.unwords (map (prettyPrintWildcard wsa) ctorArgs) <>")"

prettyPrintWildcard :: WildcardAnnotations -> P.Type -> Text
prettyPrintWildcard (WildcardAnnotations True) = prettyWildcard
prettyPrintWildcard (WildcardAnnotations False) = const "_"

prettyWildcard :: P.Type -> Text
prettyWildcard t = "( _ :: " <> T.strip (T.pack (P.prettyPrintTypeAtom t)) <> ")"

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

addClause :: (MonadError PscIdeError m) => Text -> WildcardAnnotations -> m [Text]
addClause s wca = do
  (fName, fType) <- parseTypeDeclaration' s
  let args = splitFunctionType fType
      template = runIdentT fName <> " " <>
        T.unwords (map (prettyPrintWildcard wca) args) <>
        " = ?" <> (T.strip . runIdentT $ fName)
  pure [s, template]

parseType' :: (MonadError PscIdeError m) =>
              Text -> m P.Type
parseType' s =
  case P.lex "<psc-ide>" (toS s) >>= P.runTokenParser "<psc-ide>" (P.parseType <* Parsec.eof) of
    Right type' -> pure type'
    Left err ->
      throwError (GeneralError ("Parsing the splittype failed with:"
                                <> show err))

parseTypeDeclaration' :: (MonadError PscIdeError m) => Text -> m (P.Ident, P.Type)
parseTypeDeclaration' s =
  let x = do
        ts <- P.lex "" (toS s)
        P.runTokenParser "" (P.parseDeclaration <* Parsec.eof) ts
  in
    case unwrapPositioned <$> x of
      Right (P.TypeDeclaration i t) -> pure (i, t)
      Right _ -> throwError (GeneralError "Found a non-type-declaration")
      Left err ->
        throwError (GeneralError ("Parsing the type signature failed with: "
                                   <> toS (Box.render (P.prettyPrintParseError err))))

splitFunctionType :: P.Type -> [P.Type]
splitFunctionType t = fromMaybe [] arguments
  where
    arguments = initMay splitted
    splitted = splitType' t
    splitType' (P.ForAll _ t' _) = splitType' t'
    splitType' (P.ConstrainedType _ t') = splitType' t'
    splitType' (P.TypeApp (P.TypeApp t' lhs) rhs)
          | t' == P.tyFunction = lhs : splitType' rhs
    splitType' t' = [t']
