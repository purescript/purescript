-- | Functions for converting PureScript ASTs into values of the data types
-- from Language.PureScript.Docs.

module Language.PureScript.Docs.Convert
  ( convertModule
  ) where

import Protolude hiding (check)

import Control.Category ((>>>))
import Control.Monad.Writer.Strict (runWriterT)
import Control.Monad.Supply (evalSupplyT)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.String (String)

import Language.PureScript.Docs.Convert.Single (convertSingleModule)
import Language.PureScript.Docs.Types
import qualified Language.PureScript.CST as CST
import qualified Language.PureScript.AST as P
import qualified Language.PureScript.Crash as P
import qualified Language.PureScript.Errors as P
import qualified Language.PureScript.Externs as P
import qualified Language.PureScript.Environment as P
import qualified Language.PureScript.Names as P
import qualified Language.PureScript.Sugar as P
import qualified Language.PureScript.Types as P

-- |
-- Convert a single module to a Docs.Module, making use of a pre-existing
-- type-checking environment in order to fill in any missing types. Note that
-- re-exports will not be included.
--
convertModule ::
  MonadError P.MultipleErrors m =>
  [P.ExternsFile] ->
  P.Env ->
  P.Environment ->
  P.Module ->
  m Module
convertModule externs env checkEnv =
  fmap (insertValueTypesAndInferredKinds checkEnv . convertSingleModule) . partiallyDesugar externs env

-- |
-- Updates all the types of the ValueDeclarations inside the module based on
-- their types inside the given Environment.
--
-- Also inserts inferred kind signatures into the corresponding declarations
-- if no kind signatures were declared explicitly.
--
insertValueTypesAndInferredKinds ::
  P.Environment -> Module -> Module
insertValueTypesAndInferredKinds env m =
  m { modDeclarations = map go (modDeclarations m) }
  where
  -- insert value types
  go d@Declaration { declInfo = ValueDeclaration P.TypeWildcard{} } =
    let
      ident = P.Ident . CST.getIdent . CST.nameValue . parseIdent $ declTitle d
      ty = lookupName ident
    in
      d { declInfo = ValueDeclaration (ty $> ()) }

  -- insert inferred kinds
  go d@Declaration{..} | isNothing declKind = case declInfo of
    DataDeclaration dataDeclType _ -> do
      d { declKind = mkInferredKindInfo declTitle $ toKindSignatureFor dataDeclType }

    TypeSynonymDeclaration _ _ ->
      d { declKind = mkInferredKindInfo declTitle P.TypeSynonymSig }

    TypeClassDeclaration _ _ _ ->
      d { declKind = mkInferredKindInfo declTitle P.ClassSig }

    _ -> d

  go other =
    other

  parseIdent =
    either (err . ("failed to parse Ident: " ++)) identity . runParser CST.parseIdent

  lookupName name =
    let key = P.Qualified (Just (modName m)) name
    in case Map.lookup key (P.names env) of
      Just (ty, _, _) ->
        ty
      Nothing ->
        err ("name not found: " ++ show key)

  toKindSignatureFor :: P.DataDeclType -> P.KindSignatureFor
  toKindSignatureFor = \case
    P.Data -> P.DataSig
    P.Newtype -> P.NewtypeSig

  mkInferredKindInfo :: Text -> P.KindSignatureFor -> Maybe KindInfo
  mkInferredKindInfo name keyword =
    let key = P.Qualified (Just (modName m)) (P.ProperName name)
    in case Map.lookup key (P.types env) of
      Just (inferredKind, _) ->
        Just $ KindInfo { kiKeyword = keyword, kiKind = () <$ inferredKind }
      Nothing ->
        err ("type not found: " ++ show key)

  err msg =
    P.internalError ("Docs.Convert.insertValueTypes: " ++ msg)

runParser :: CST.Parser a -> Text -> Either String a
runParser p =
  bimap (CST.prettyPrintError . NE.head) snd
    . CST.runTokenParser p
    . CST.lex

-- |
-- Partially desugar modules so that they are suitable for extracting
-- documentation information from.
--
partiallyDesugar ::
  (MonadError P.MultipleErrors m) =>
  [P.ExternsFile] ->
  P.Env ->
  P.Module ->
  m P.Module
partiallyDesugar externs env = evalSupplyT 0 . desugar'
  where
  desugar' =
    P.desugarDoModule
      >=> P.desugarAdoModule
      >=> P.desugarLetPatternModule
      >>> P.desugarCasesModule
      >=> P.desugarTypeDeclarationsModule
      >=> fmap fst . runWriterT . flip evalStateT (env, mempty) . P.desugarImports
      >=> P.rebracketFiltered isInstanceDecl externs

  isInstanceDecl P.TypeInstanceDeclaration {} = True
  isInstanceDecl _ = False
