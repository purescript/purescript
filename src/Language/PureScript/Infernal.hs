{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -fno-warn-unused-binds #-}

module Language.PureScript.Infernal
  ( typeSearch
  , typeSearch'
  ) where

import           Protolude

import           Control.Monad.Writer
import           Data.Aeson (decodeStrict)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.PureScript.TypeChecker.Entailment as Entailment

import qualified Language.PureScript.TypeChecker.Monad      as TC
import           Language.PureScript.TypeChecker.Subsumption
import           Language.PureScript.TypeChecker.Unify as P

import Language.PureScript.AST as P
import Language.PureScript.Crash
import Control.Monad.Supply as P
import Language.PureScript.Environment as P
import Language.PureScript.Errors as P
import Language.PureScript.Names as P
import Language.PureScript.Externs as P
import Language.PureScript.Types as P
import Language.PureScript.TypeChecker.Skolems as Skolem
import Language.PureScript.TypeChecker.Synonyms as P
import Language.PureScript.ModuleDependencies as P

-- REPL functions
loadModules :: [Text] -> IO P.Environment
loadModules = loadEnv . map (\m -> "./externs/" <> toS m <> ".json")

loadEnv :: MonadIO m => [FilePath] -> m P.Environment
loadEnv modules = do
  mkEnv <$> traverse readExternFile modules
  where
    readExternFile :: MonadIO m => FilePath -> m P.ExternsFile
    readExternFile fp = do
      parseResult <- liftIO (decodeStrict <$> BS.readFile fp)
      case parseResult of
        Nothing -> internalError "parsing an extern failed"
        Just externs -> pure externs

-- Potentially needed for psc-ide integration

mkEnv :: [P.ExternsFile] -> P.Environment
mkEnv externs =
  let externsMap = foldr (\e -> Map.insert (P.efModuleName e) e) Map.empty externs
  in foldl' (flip P.applyExternsFileToEnvironment) P.initEnvironment (sortExterns externsMap)

sortExterns :: Map P.ModuleName P.ExternsFile -> [P.ExternsFile]
sortExterns externs = do
  sorted' <- runExceptT . P.sortModules . map mkShallowModule . Map.elems $ externs
  case sorted' of
    Left _ -> internalError "There was a cycle in the dependencies"
    Right (sorted, _) -> do
      mapMaybe getExtern ((P.efModuleName <$> Map.elems externs) `inOrderOf` (P.getModuleName <$> sorted))
  where
    mkShallowModule P.ExternsFile{..} =
      P.Module (P.internalModuleSourceSpan "<rebuild>") [] efModuleName (map mkImport efImports) Nothing
    mkImport (P.ExternsImport mn it iq) =
      P.ImportDeclaration mn it iq
    getExtern = flip Map.lookup externs
    -- Sort a list so its elements appear in the same order as in another list.
    inOrderOf :: (Ord a) => [a] -> [a] -> [a]
    inOrderOf xs ys = let s = Set.fromList xs in filter (`Set.member` s) ys

------

xrunSubsume
  :: Environment
  -> StateT TC.CheckState (SupplyT (WriterT b (Except e))) a
  -> Either e (a, Environment)
xrunSubsume env = runExcept . evalWriterT . P.evalSupplyT 0 . TC.runCheck' env

evalWriterT :: Monad m => WriterT b m r -> m r
evalWriterT m = liftM fst (runWriterT m)

filtering
  :: P.Environment
  -- ^ The Environment which contains the relevant definitions and typeclasses
  -> P.Type
  -- ^ The type supplied by the environment
  -> P.Type
  -- ^ The user supplied type
  -> Either P.MultipleErrors ((P.Expr, [(P.Ident, P.Constraint)]), P.Environment)
filtering env x t = xrunSubsume env $ do
  let initializeSkolems = Skolem.introduceSkolemScope <=< P.replaceAllTypeSynonyms <=< P.replaceTypeWildcards

  x' <- initializeSkolems x
  t' <- initializeSkolems t

  let dummyExpression = P.Var (P.Qualified Nothing (P.Ident "x"))
  elab <- runExceptT $ subsumes (Just dummyExpression) t' x'
  subst <- gets TC.checkSubstitution
  case elab of
    Left _ -> throwError undefined
    Right (Just expP) -> do
      let expPP = overTypes (P.substituteType subst) expP
      Entailment.replaceTypeClassDictionaries True (P.moduleNameFromString "Dx") expPP
    Right Nothing -> throwError undefined

typeSearch
  :: P.Environment
  -> P.Type
  -> Map (P.Qualified P.Ident) (P.Type, P.NameKind, P.NameVisibility)
typeSearch env type' =
  Map.filter (\(x, _, _) -> isRight (filtering env x type')) (P.names env)

typeSearch'
  :: P.Environment
  -> P.Type
  -> Map (P.Qualified P.Ident) P.Type
typeSearch' env type' =
  Map.mapMaybe (\(x, _, _) -> if isRight (filtering env type' x)
                              then Just x
                              else Nothing) (P.names env)

overTypes :: (P.Type -> P.Type) -> P.Expr -> P.Expr
overTypes f = let (_, f', _) = P.everywhereOnValues identity g identity in f'
  where
  g :: P.Expr -> P.Expr
  g (P.TypedValue checkTy val t) = P.TypedValue checkTy val (f t)
  g (P.TypeClassDictionary c sco hints) = P.TypeClassDictionary (P.mapConstraintArgs (map f) c) sco hints
  g other = other
