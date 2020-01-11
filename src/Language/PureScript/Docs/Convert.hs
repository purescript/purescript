-- | Functions for converting PureScript ASTs into values of the data types
-- from Language.PureScript.Docs.

module Language.PureScript.Docs.Convert
  ( convertModule
  ) where

import Protolude hiding (check)

import Control.Category ((>>>))
import Control.Monad.Writer.Strict (runWriterT)
import Control.Monad.Supply (evalSupplyT)
import Data.Functor (($>))
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
convertModule externs env checkEnv m =
  partiallyDesugar externs env [m] >>= \case
    [m'] -> pure (insertValueTypes checkEnv (convertSingleModule m'))
    _ -> P.internalError "partiallyDesugar did not return a singleton"

-- |
-- Updates all the types of the ValueDeclarations inside the module based on
-- their types inside the given Environment.
--
insertValueTypes ::
  P.Environment -> Module -> Module
insertValueTypes env m =
  m { modDeclarations = map go (modDeclarations m) }
  where
  go (d@Declaration { declInfo = ValueDeclaration P.TypeWildcard{} }) =
    let
      ident = P.Ident . CST.getIdent . CST.nameValue . parseIdent $ declTitle d
      ty = lookupName ident
    in
      d { declInfo = ValueDeclaration (ty $> ()) }
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

  err msg =
    P.internalError ("Docs.Convert.insertValueTypes: " ++ msg)

runParser :: CST.Parser a -> Text -> Either String a
runParser p =
  first (CST.prettyPrintError . NE.head)
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
  [P.Module] ->
  m [P.Module]
partiallyDesugar externs env = evalSupplyT 0 . desugar'
  where
  desugar' =
    traverse P.desugarDoModule
      >=> traverse P.desugarAdoModule
      >=> map P.desugarLetPatternModule
      >>> traverse P.desugarCasesModule
      >=> traverse P.desugarTypeDeclarationsModule
      >=> fmap fst . runWriterT . P.desugarImports env
      >=> P.rebracketFiltered isInstanceDecl externs

  isInstanceDecl (P.TypeInstanceDeclaration {}) = True
  isInstanceDecl _ = False
