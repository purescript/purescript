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

import Language.PureScript.AST.Declarations qualified as ASTD
import Language.PureScript.Docs.Convert.Single (convertSingleModule)
import Language.PureScript.Docs.Types ( Declaration(..), DeclarationInfo(TypeClassDeclaration, ExternDataDeclaration, ValueDeclaration, DataDeclaration, TypeSynonymDeclaration), KindInfo(KindInfo, kiKind, kiKeyword), Module(modDeclarations, modName), Type' )
import Language.PureScript.CST.Errors ( prettyPrintError )
import Language.PureScript.CST.Lexer ( lex )
import Language.PureScript.CST.Monad ( runTokenParser, Parser )
import Language.PureScript.CST.Parser qualified as CSTParser
import Language.PureScript.CST.Types ( Ident(getIdent), Name(nameValue) )
import Language.PureScript.Crash (internalError)
import Language.PureScript.Errors (MultipleErrors)
import Language.PureScript.Externs (ExternsFile)
import Language.PureScript.Environment qualified as PEnv
import Language.PureScript.Names qualified as PN
import Language.PureScript.Roles (Role)
import Language.PureScript.Sugar.AdoNotation ( desugarAdoModule )
import Language.PureScript.Sugar.CaseDeclarations ( desugarCasesModule )
import Language.PureScript.Sugar.DoNotation ( desugarDoModule )
import Language.PureScript.Sugar.LetPattern ( desugarLetPatternModule )
import Language.PureScript.Sugar.Names ( Env, desugarImports )
import Language.PureScript.Sugar.Operators ( rebracketFiltered )
import Language.PureScript.Sugar.TypeDeclarations ( desugarTypeDeclarationsModule )
import Language.PureScript.Types qualified as PT
import Language.PureScript.Constants.Prim qualified as CPrim
import Language.PureScript.Sugar.Operators ( RebracketCaller(CalledByDocs) )

-- |
-- Convert a single module to a Docs.Module, making use of a pre-existing
-- type-checking environment in order to fill in any missing types. Note that
-- re-exports will not be included.
--
convertModule ::
  MonadError MultipleErrors m =>
  [ExternsFile] ->
  Env ->
  PEnv.Environment ->
  ASTD.Module ->
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
  PEnv.Environment -> Module -> Module
insertValueTypesAndAdjustKinds env m =
  m { modDeclarations = map (go . insertInferredRoles . convertFFIDecl) (modDeclarations m) }
  where
  -- |
  -- Convert FFI declarations into data declaration
  -- by generating the type parameters' names based on its kind signature.
  -- Note: `Prim` modules' docs don't go through this conversion process
  -- so `ExternDataDeclaration` values will still exist beyond this point.
  convertFFIDecl d@Declaration { declInfo = ExternDataDeclaration kind roles } =
    d { declInfo = DataDeclaration PEnv.Data (genTypeParams kind) roles
      , declKind = Just (KindInfo ASTD.DataSig kind)
      }

  convertFFIDecl other = other

  insertInferredRoles d@Declaration { declInfo = DataDeclaration dataDeclType args [] } =
    d { declInfo = DataDeclaration dataDeclType args inferredRoles }

    where
    inferredRoles :: [Role]
    inferredRoles = do
      let key = PN.Qualified (PN.ByModuleName (modName m)) (PN.ProperName (declTitle d))
      case Map.lookup key (PEnv.types env) of
        Just (_, tyKind) -> case tyKind of
          PEnv.DataType _ tySourceTyRole _ ->
            map (\(_,_,r) -> r) tySourceTyRole
          PEnv.ExternData rs ->
            rs
          _ ->
            []
        Nothing ->
          err $ "type not found: " <> show key

  insertInferredRoles other =
    other

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
  genTypeParams :: Type' -> [(Text, Maybe Type')]
  genTypeParams kind = do
    let n = countParams 0 kind
    map (\(i :: Int) -> ("t" <> T.pack (show i), Nothing)) $ take n [0..]
    where
      countParams :: Int -> Type' -> Int
      countParams acc = \case
        PT.ForAll _ _ _ rest _ ->
          countParams acc rest

        PT.TypeApp _ f a | isFunctionApplication f ->
          countParams (acc + 1) a

        PT.ParensInType _ ty ->
          countParams acc ty

        _ ->
          acc

      isFunctionApplication = \case
        PT.TypeApp _ (PT.TypeConstructor () CPrim.Function) _ -> True
        PT.ParensInType _ ty -> isFunctionApplication ty
        _ -> False

  -- insert value types
  go d@Declaration { declInfo = ValueDeclaration PT.TypeWildcard{} } =
    let
      ident = PN.Ident . getIdent . nameValue . parseIdent $ declTitle d
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
    either (err . ("failed to parse Ident: " ++)) identity . runParser CSTParser.parseIdent

  lookupName name =
    let key = PN.Qualified (PN.ByModuleName (modName m)) name
    in case Map.lookup key (PEnv.names env) of
      Just (ty, _, _) ->
        ty
      Nothing ->
        err ("name not found: " ++ show key)

  -- |
  -- Extracts the keyword for a declaration (if there is one)
  extractKeyword :: DeclarationInfo -> Maybe ASTD.KindSignatureFor
  extractKeyword = \case
    DataDeclaration dataDeclType _ _ -> Just $ case dataDeclType of
      PEnv.Data -> ASTD.DataSig
      PEnv.Newtype -> ASTD.NewtypeSig
    TypeSynonymDeclaration _ _ -> Just ASTD.TypeSynonymSig
    TypeClassDeclaration _ _ _ -> Just ASTD.ClassSig
    _ -> Nothing

  -- |
  -- Returns True if the kind signature is "uninteresting", which
  -- is a kind that follows this form:
  -- - `Type`
  -- - `Constraint` (class declaration only)
  -- - `Type -> K` where `K` is an "uninteresting" kind
  isUninteresting
    :: ASTD.KindSignatureFor -> Type' -> Bool
  isUninteresting keyword = \case
    -- `Type -> ...`
    PT.TypeApp _ f a | isTypeAppFunctionType f -> isUninteresting keyword a
    PT.ParensInType _ ty -> isUninteresting keyword ty
    x -> isKindPrimType x || (isClassKeyword && isKindPrimConstraint x)
    where
      isClassKeyword = case keyword of
        ASTD.ClassSig -> True
        _ -> False

      isTypeAppFunctionType = \case
        PT.TypeApp _ f a -> isKindFunction f && isKindPrimType a
        PT.ParensInType _ ty -> isTypeAppFunctionType ty
        _ -> False

      isKindFunction = isTypeConstructor CPrim.Function
      isKindPrimType = isTypeConstructor CPrim.Type
      isKindPrimConstraint = isTypeConstructor CPrim.Constraint

      isTypeConstructor k = \case
        PT.TypeConstructor _ k' -> k' == k
        PT.ParensInType _ ty -> isTypeConstructor k ty
        _ -> False

  insertInferredKind :: Declaration -> Text -> ASTD.KindSignatureFor -> Declaration
  insertInferredKind d name keyword =
    let
      key = PN.Qualified (PN.ByModuleName (modName m)) (PN.ProperName name)
    in case Map.lookup key (PEnv.types env) of
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
            PT.ForAll sa txt (Just (PT.TypeConstructor _ CPrim.Type)) rest skol ->
              PT.ForAll sa txt Nothing (dropTypeSortAnnotation rest) skol
            rest -> rest

      Nothing ->
        err ("type not found: " ++ show key)

  err msg =
    internalError ("Docs.Convert.insertValueTypes: " ++ msg)

runParser :: Parser a -> Text -> Either String a
runParser p =
  bimap (prettyPrintError . NE.head) snd
    . runTokenParser p
    . lex

-- |
-- Partially desugar modules so that they are suitable for extracting
-- documentation information from.
--
partiallyDesugar ::
  (MonadError MultipleErrors m) =>
  [ExternsFile] ->
  Env ->
  ASTD.Module ->
  m ASTD.Module
partiallyDesugar externs env = evalSupplyT 0 . desugar'
  where
  desugar' =
    desugarDoModule
      >=> desugarAdoModule
      >=> desugarLetPatternModule
      >>> desugarCasesModule
      >=> desugarTypeDeclarationsModule
      >=> fmap fst . runWriterT . flip evalStateT (env, mempty) . desugarImports
      >=> rebracketFiltered CalledByDocs isInstanceDecl externs

  isInstanceDecl ASTD.TypeInstanceDeclaration {} = True
  isInstanceDecl _ = False
