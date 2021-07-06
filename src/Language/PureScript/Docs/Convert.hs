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
import qualified Data.Text as T

import Language.PureScript.Docs.Convert.Single (convertSingleModule)
import Language.PureScript.Docs.Types
import qualified Language.PureScript.CST as CST
import qualified Language.PureScript.AST as P
import qualified Language.PureScript.Crash as P
import qualified Language.PureScript.Errors as P
import qualified Language.PureScript.Externs as P
import qualified Language.PureScript.Environment as P
import qualified Language.PureScript.Names as P
import qualified Language.PureScript.Roles as P
import qualified Language.PureScript.Sugar as P
import qualified Language.PureScript.Types as P
import qualified Language.PureScript.Constants.Prim as Prim

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
  m { modDeclarations = map (go . convertFFIDecl) (modDeclarations m) }
  where
  -- |
  -- Convert FFI declarations into data declaration
  -- by generating the type parameters' names based on its kind signature.
  -- Note: `Prim` modules' docs don't go through this conversion process
  -- so `ExternDataDeclaration` values will still exist beyond this point.
  convertFFIDecl d@Declaration { declInfo = ExternDataDeclaration kind roles } =
    d { declInfo = DataDeclaration P.Data (genFFITypeParams kind) (getRoles roles)
      , declKind = Just (KindInfo P.DataSig kind)
      }
    where
      getRoles [] = lookupInferredRole
      getRoles r = r

      lookupInferredRole :: [P.Role]
      lookupInferredRole = fromMaybe [] $ do
        let key = P.Qualified (Just (modName m)) (P.ProperName (declTitle d))
        (_, tyKind) <- Map.lookup key (P.types env)
        case tyKind of
          P.DataType _ tySourceTyRole _ ->
            Just $ map (\(_,_,r) -> r) tySourceTyRole
          P.ExternData rs ->
            Just rs
          _ -> Nothing

  convertFFIDecl other = other

  -- |
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
  genFFITypeParams :: Type' -> [(Text, Maybe Type')]
  genFFITypeParams kind = do
    let n = countParams 0 kind
    if n == 0
      then []
      else map (\i -> ("t" <> T.pack (show i), Nothing)) [0..n]
    where
      countParams :: Integer -> Type' -> Integer
      countParams acc = \case
        -- skip foralls
        P.ForAll _ _ _ rest _ ->
          countParams acc rest

        -- increase count whenever we come across
        -- `SomeKind -> ...`
        P.TypeApp _ f a@(P.TypeApp _ _ _) | isFunctionApplication f ->
          countParams (acc + 1) a

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
    let key = P.Qualified (Just (modName m)) name
    in case Map.lookup key (P.names env) of
      Just (ty, _, _) ->
        ty
      Nothing ->
        err ("name not found: " ++ show key)

  -- |
  -- Extracts the keyword for a declaration (if there is one)
  extractKeyword :: DeclarationInfo -> Maybe P.KindSignatureFor
  extractKeyword = \case
    DataDeclaration dataDeclType _ _ -> Just $ case dataDeclType of
      P.Data -> P.DataSig
      P.Newtype -> P.NewtypeSig
    TypeSynonymDeclaration _ _ -> Just P.TypeSynonymSig
    TypeClassDeclaration _ _ _ -> Just P.ClassSig
    _ -> Nothing

  -- |
  -- Returns True if the kind signature is "uninteresting", which
  -- is a kind that follows this form:
  -- - `Type`
  -- - `Constraint` (class declaration only)
  -- - `Type -> K` where `K` is an "uninteresting" kind
  isUninteresting
    :: P.KindSignatureFor -> P.Type () -> Bool
  isUninteresting keyword = \case
    P.TypeApp _ t1 t2 | t1 == kindFunctionType ->
      isUninteresting keyword t2
    x ->
      x == kindPrimType || (isClassKeyword && x == kindPrimConstraint)
    where
      isClassKeyword = case keyword of
        P.ClassSig -> True
        _ -> False

  insertInferredKind :: Declaration -> Text -> P.KindSignatureFor -> Declaration
  insertInferredKind d name keyword =
    let
      key = P.Qualified (Just (modName m)) (P.ProperName name)
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

          -- changes `forall (k :: Type). k -> ...`
          -- to      `forall k          . k -> ...`
          dropTypeSortAnnotation = \case
            P.ForAll sa txt (Just kAnn) rest skol | kAnn == kindPrimType ->
              P.ForAll sa txt Nothing (dropTypeSortAnnotation rest) skol
            rest -> rest

      Nothing ->
        err ("type not found: " ++ show key)

  -- constants for kind signature-related code
  kindPrimType = P.TypeConstructor () Prim.Type
  kindPrimFunction = P.TypeConstructor () Prim.Function
  kindPrimConstraint = P.TypeConstructor () Prim.Constraint
  -- `Type ->`
  kindFunctionType = P.TypeApp () kindPrimFunction kindPrimType

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
