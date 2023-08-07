-- | Functions for converting PureScript ASTs into values of the data types
-- from Language.PureScript.Docs.

module Language.PureScript.Docs.Convert
  ( convertModule
  ) where

import Protolude hiding (check)

import Control.Category ((>>>))
import Control.Monad.Writer.Strict (runWriterT)
import Control.Monad.Supply (evalSupplyT)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.String (String)
import Data.Text qualified as T

import Language.PureScript.Docs.Convert.Single (convertSingleModule)
import Language.PureScript.Docs.Types (Declaration(..), DeclarationInfo(..), KindInfo(..), Module(..), Type')
import Language.PureScript.CST qualified as CST
import Language.PureScript.AST qualified as P
import Language.PureScript.Crash qualified as P
import Language.PureScript.Errors qualified as P
import Language.PureScript.Externs qualified as P
import Language.PureScript.Environment qualified as P
import Language.PureScript.Names qualified as P
import Language.PureScript.Roles qualified as P
import Language.PureScript.Sugar qualified as P
import Language.PureScript.Types qualified as P
import Language.PureScript.Constants.Prim qualified as Prim
import Language.PureScript.Sugar (RebracketCaller(CalledByDocs))

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
  fmap (insertValueTypesAndAdjustKinds checkEnv . convertSingleModule) . partiallyDesugar externs env

-- |
-- Convert FFI declarations into `DataDeclaration` so that the declaration's
-- roles (if any) can annotate the generated type parameter names.
--
-- Inserts all data declarations inferred roles if none were specified
-- explicitly.
--
-- Updates all the types of the ValueDeclarations inside the module based on
-- their types inside the given Environment.
--
-- Removes explicit kind signatures if they are "uninteresting."
--
-- Inserts inferred kind signatures into the corresponding declarations
-- if no kind signature was declared explicitly and the kind
-- signature is "interesting."
--
insertValueTypesAndAdjustKinds ::
  P.Environment -> Module -> Module
insertValueTypesAndAdjustKinds env m =
  m { modDeclarations = map (go . insertInferredRoles . convertFFIDecl) (modDeclarations m) }
  where
  -- Convert FFI declarations into data declaration
  -- by generating the type parameters' names based on its kind signature.
  -- Note: `Prim` modules' docs don't go through this conversion process
  -- so `ExternDataDeclaration` values will still exist beyond this point.
  convertFFIDecl d@Declaration { declInfo = ExternDataDeclaration kind roles } =
    d { declInfo = DataDeclaration P.Data (genTypeParams kind) roles
      , declKind = Just (KindInfo P.DataSig kind)
      }

  convertFFIDecl other = other

  insertInferredRoles d@Declaration { declInfo = DataDeclaration dataDeclType args [] } =
    d { declInfo = DataDeclaration dataDeclType args inferredRoles }

    where
    inferredRoles :: [P.Role]
    inferredRoles = do
      let key = P.Qualified (P.ByModuleName (modName m)) (P.ProperName (declTitle d))
      case Map.lookup key (P.types env) of
        Just (_, tyKind) -> case tyKind of
          P.DataType _ tySourceTyRole _ ->
            map (\(_,_,r) -> r) tySourceTyRole
          P.ExternData rs ->
            rs
          _ ->
            []
        Nothing ->
          err $ "type not found: " <> show key

  insertInferredRoles other =
    other

  -- Given an FFI declaration like this
  -- ```
  -- foreign import data Foo
  --    :: forall a b c d
  --     . MyKind a b
  --    -> OtherKind c d
  --    -> Symbol
  --    -> (Type -> Type)
  --    -> (Type) -- unneeded parens a developer typo
  --    -> Type
  -- ```
  -- Return a list of values, one for each implicit type parameter
  -- of `(tX, Nothing)` where `X` refers to the index of he parameter
  -- in that list, matching the values expected by `Render.toTypeVar`
  genTypeParams :: Type' -> [(Text, Maybe Type')]
  genTypeParams kind = do
    let n = countParams 0 kind
    map (\(i :: Int) -> ("t" <> T.pack (show i), Nothing)) $ take n [0..]
    where
      countParams :: Int -> Type' -> Int
      countParams acc = \case
        P.ForAll _ _ _ _ rest _ ->
          countParams acc rest

        P.TypeApp _ f a | isFunctionApplication f ->
          countParams (acc + 1) a

        P.ParensInType _ ty ->
          countParams acc ty

        _ ->
          acc

      isFunctionApplication = \case
        P.TypeApp _ (P.TypeConstructor () Prim.Function) _ -> True
        P.ParensInType _ ty -> isFunctionApplication ty
        _ -> False

  -- insert value types
  go d@Declaration { declInfo = ValueDeclaration P.TypeWildcard{} } =
    let
      ident = P.Ident . CST.getIdent . CST.nameValue . parseIdent $ declTitle d
      ty = lookupName ident
    in
      d { declInfo = ValueDeclaration (ty $> ()) }

  go d@Declaration{..} | Just keyword <- extractKeyword declInfo =
    case declKind of
      Just ks ->
        -- hide explicit kind signatures that are "uninteresting"
        if isUninteresting keyword $ kiKind ks
          then d { declKind = Nothing }
          else d
      Nothing ->
        -- insert inferred kinds so long as they are "interesting"
        insertInferredKind d declTitle keyword

  go other =
    other

  parseIdent =
    either (err . ("failed to parse Ident: " ++)) identity . runParser CST.parseIdent

  lookupName name =
    let key = P.Qualified (P.ByModuleName (modName m)) name
    in case Map.lookup key (P.names env) of
      Just (ty, _, _) ->
        ty
      Nothing ->
        err ("name not found: " ++ show key)

  -- Extracts the keyword for a declaration (if there is one)
  extractKeyword :: DeclarationInfo -> Maybe P.KindSignatureFor
  extractKeyword = \case
    DataDeclaration dataDeclType _ _ -> Just $ case dataDeclType of
      P.Data -> P.DataSig
      P.Newtype -> P.NewtypeSig
    TypeSynonymDeclaration _ _ -> Just P.TypeSynonymSig
    TypeClassDeclaration _ _ _ -> Just P.ClassSig
    _ -> Nothing

  -- Returns True if the kind signature is "uninteresting", which
  -- is a kind that follows this form:
  -- - `Type`
  -- - `Constraint` (class declaration only)
  -- - `Type -> K` where `K` is an "uninteresting" kind
  isUninteresting
    :: P.KindSignatureFor -> Type' -> Bool
  isUninteresting keyword = \case
    -- `Type -> ...`
    P.TypeApp _ f a | isTypeAppFunctionType f -> isUninteresting keyword a
    P.ParensInType _ ty -> isUninteresting keyword ty
    x -> isKindPrimType x || (isClassKeyword && isKindPrimConstraint x)
    where
      isClassKeyword = case keyword of
        P.ClassSig -> True
        _ -> False

      isTypeAppFunctionType = \case
        P.TypeApp _ f a -> isKindFunction f && isKindPrimType a
        P.ParensInType _ ty -> isTypeAppFunctionType ty
        _ -> False

      isKindFunction = isTypeConstructor Prim.Function
      isKindPrimType = isTypeConstructor Prim.Type
      isKindPrimConstraint = isTypeConstructor Prim.Constraint

      isTypeConstructor k = \case
        P.TypeConstructor _ k' -> k' == k
        P.ParensInType _ ty -> isTypeConstructor k ty
        _ -> False

  insertInferredKind :: Declaration -> Text -> P.KindSignatureFor -> Declaration
  insertInferredKind d name keyword =
    let
      key = P.Qualified (P.ByModuleName (modName m)) (P.ProperName name)
    in case Map.lookup key (P.types env) of
      Just (inferredKind, _) ->
        if isUninteresting keyword inferredKind'
          then  d
          else  d { declKind = Just $ KindInfo
                    { kiKeyword = keyword
                    , kiKind = dropTypeSortAnnotation inferredKind'
                    }
                  }
        where
          inferredKind' = inferredKind $> ()

          -- Note: the below change to the final kind used is intentionally
          -- NOT being done for explicit kind signatures:
          --
          -- changes `forall (k :: Type). k -> ...`
          -- to      `forall k          . k -> ...`
          dropTypeSortAnnotation = \case
            P.ForAll sa vis txt (Just (P.TypeConstructor _ Prim.Type)) rest skol ->
              P.ForAll sa vis txt Nothing (dropTypeSortAnnotation rest) skol
            rest -> rest

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
      >=> P.rebracketFiltered CalledByDocs isInstanceDecl externs

  isInstanceDecl P.TypeInstanceDeclaration {} = True
  isInstanceDecl _ = False
