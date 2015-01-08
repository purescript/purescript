-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.BindingGroups
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module implements the desugaring pass which creates binding groups from sets of
-- mutually-recursive value declarations and mutually-recursive type declarations.
--
-----------------------------------------------------------------------------

module Language.PureScript.Sugar.BindingGroups (
    createBindingGroups,
    createBindingGroupsModule,
    collapseBindingGroups,
    collapseBindingGroupsModule
) where

import Data.Graph
import Data.List (nub, intersect)
import Data.Maybe (isJust, mapMaybe)
import Data.Monoid ((<>))
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad ((<=<))

import qualified Data.Set as S

import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Environment
import Language.PureScript.Errors

-- |
-- Replace all sets of mutually-recursive declarations in a module with binding groups
--
createBindingGroupsModule :: [Module] -> Either ErrorStack [Module]
createBindingGroupsModule = mapM $ \(Module name ds exps) -> Module name <$> createBindingGroups name ds <*> pure exps

-- |
-- Collapse all binding groups in a module to individual declarations
--
collapseBindingGroupsModule :: [Module] -> [Module]
collapseBindingGroupsModule = map $ \(Module name ds exps) -> Module name (collapseBindingGroups ds) exps

createBindingGroups :: ModuleName -> [Declaration] -> Either ErrorStack [Declaration]
createBindingGroups moduleName = mapM f <=< handleDecls

  where
  (f, _, _) = everywhereOnValuesTopDownM return handleExprs return 
      
  handleExprs :: Expr -> Either ErrorStack Expr
  handleExprs (Let ds val) = flip Let val <$> handleDecls ds
  handleExprs other = return other
  
  -- |
  -- Replace all sets of mutually-recursive declarations with binding groups
  --
  handleDecls :: [Declaration] -> Either ErrorStack [Declaration]
  handleDecls ds = do
    let values = filter isValueDecl ds
        dataDecls = filter isDataDecl ds
        allProperNames = map getProperName dataDecls
        dataVerts = map (\d -> (d, getProperName d, usedProperNames moduleName d `intersect` allProperNames)) dataDecls
    dataBindingGroupDecls <- parU (stronglyConnComp dataVerts) toDataBindingGroup
    let allIdents = map getIdent values
        valueVerts = map (\d -> (d, getIdent d, usedIdents moduleName d `intersect` allIdents)) values
    bindingGroupDecls <- parU (stronglyConnComp valueVerts) (toBindingGroup moduleName)
    return $ filter isImportDecl ds ++
             filter isExternDataDecl ds ++
             filter isExternInstanceDecl ds ++
             dataBindingGroupDecls ++
             filter isTypeClassDeclaration ds ++
             filter isFixityDecl ds ++
             filter isExternDecl ds ++
             bindingGroupDecls

-- |
-- Collapse all binding groups to individual declarations
--
collapseBindingGroups :: [Declaration] -> [Declaration]
collapseBindingGroups = let (f, _, _) = everywhereOnValues id collapseBindingGroupsForValue id in map f . concatMap go
  where
  go (DataBindingGroupDeclaration ds) = ds
  go (BindingGroupDeclaration ds) = map (\(ident, nameKind, val) -> ValueDeclaration ident nameKind [] (Right val)) ds
  go (PositionedDeclaration pos com d) = map (PositionedDeclaration pos com) $ go d
  go other = [other]

collapseBindingGroupsForValue :: Expr -> Expr
collapseBindingGroupsForValue (Let ds val) = Let (collapseBindingGroups ds) val
collapseBindingGroupsForValue other = other

usedIdents :: ModuleName -> Declaration -> [Ident]
usedIdents moduleName =
  let (f, _, _, _, _) = everythingWithContextOnValues S.empty [] (++) def usedNamesE usedNamesB def def
  in nub . f
  where
  def s _ = (s, [])

  usedNamesE :: S.Set Ident -> Expr -> (S.Set Ident, [Ident])
  usedNamesE scope (Var (Qualified Nothing name)) | name `S.notMember` scope = (scope, [name])
  usedNamesE scope (Var (Qualified (Just moduleName') name)) | moduleName == moduleName' && name `S.notMember` scope = (scope, [name])
  usedNamesE scope (Abs (Left name) _) = (name `S.insert` scope, [])
  usedNamesE scope _ = (scope, [])

  usedNamesB :: S.Set Ident -> Binder -> (S.Set Ident, [Ident])
  usedNamesB scope binder = (scope `S.union` S.fromList (binderNames binder), [])

usedImmediateIdents :: ModuleName -> Declaration -> [Ident]
usedImmediateIdents moduleName =
  let (f, _, _, _, _) = everythingWithContextOnValues True [] (++) def usedNamesE def def def
  in nub . f
  where
  def s _ = (s, [])

  usedNamesE :: Bool -> Expr -> (Bool, [Ident])
  usedNamesE True (Var (Qualified Nothing name)) = (True, [name])
  usedNamesE True (Var (Qualified (Just moduleName') name)) | moduleName == moduleName' = (True, [name])
  usedNamesE True (Abs _ _) = (False, [])
  usedNamesE scope _ = (scope, [])

usedProperNames :: ModuleName -> Declaration -> [ProperName]
usedProperNames moduleName =
  let (f, _, _, _, _) = accumTypes (everythingOnTypes (++) usedNames)
  in nub . f
  where
  usedNames :: Type -> [ProperName]
  usedNames (ConstrainedType constraints _) = flip mapMaybe constraints $ \qual ->
    case qual of
      (Qualified (Just moduleName') name, _) | moduleName == moduleName' -> Just name
      _ -> Nothing
  usedNames (TypeConstructor (Qualified (Just moduleName') name)) | moduleName == moduleName' = [name]
  usedNames _ = []

getIdent :: Declaration -> Ident
getIdent (ValueDeclaration ident _ _ _) = ident
getIdent (PositionedDeclaration _ _ d) = getIdent d
getIdent _ = error "Expected ValueDeclaration"

getProperName :: Declaration -> ProperName
getProperName (DataDeclaration _ pn _ _) = pn
getProperName (TypeSynonymDeclaration pn _ _) = pn
getProperName (PositionedDeclaration _ _ d) = getProperName d
getProperName _ = error "Expected DataDeclaration"

-- |
-- Convert a group of mutually-recursive dependencies into a BindingGroupDeclaration (or simple ValueDeclaration).
--
--
toBindingGroup :: ModuleName -> SCC Declaration -> Either ErrorStack Declaration
toBindingGroup _ (AcyclicSCC d) = return d
toBindingGroup _ (CyclicSCC [d]) = return d
toBindingGroup moduleName (CyclicSCC ds') =
  -- Once we have a mutually-recursive group of declarations, we need to sort
  -- them further by their immediate dependencies (those outside function
  -- bodies). In particular, this is relevant for type instance dictionaries
  -- whose members require other type instances (for example, functorEff
  -- defines (<$>) = liftA1, which depends on applicativeEff). Note that
  -- superclass references are still inside functions, so don't count here.
  -- If we discover declarations that still contain mutually-recursive
  -- immediate references, we're guaranteed to get an undefined reference at
  -- runtime, so treat this as an error. See also github issue #365.
  BindingGroupDeclaration <$> mapM toBinding (stronglyConnComp valueVerts)
  where
  idents :: [Ident]
  idents = map (\(_, i, _) -> i) valueVerts

  valueVerts :: [(Declaration, Ident, [Ident])]
  valueVerts = map (\d -> (d, getIdent d, usedImmediateIdents moduleName d `intersect` idents)) ds'

  toBinding :: SCC Declaration -> Either ErrorStack (Ident, NameKind, Expr)
  toBinding (AcyclicSCC d) = return $ fromValueDecl d
  toBinding (CyclicSCC ~(d:ds)) = cycleError d ds

  cycleError :: Declaration -> [Declaration] -> Either ErrorStack a
  cycleError (PositionedDeclaration p _ d) ds = rethrowWithPosition p $ cycleError d ds
  cycleError (ValueDeclaration n _ _ (Right e)) [] = Left $
    mkErrorStack ("Cycle in definition of " ++ show n) (Just (ExprError e))
  cycleError d ds@(_:_) = rethrow (<> mkErrorStack ("The following are not yet defined here: " ++ unwords (map (show . getIdent) ds)) Nothing) $ cycleError d []
  cycleError _ _ = error "Expected ValueDeclaration"

toDataBindingGroup :: SCC Declaration -> Either ErrorStack Declaration
toDataBindingGroup (AcyclicSCC d) = return d
toDataBindingGroup (CyclicSCC [d]) = case isTypeSynonym d of
  Just pn -> Left $ mkErrorStack ("Cycle in type synonym " ++ show pn) Nothing
  _ -> return d
toDataBindingGroup (CyclicSCC ds')
  | all (isJust . isTypeSynonym) ds' = Left $ mkErrorStack "Cycle in type synonyms" Nothing
  | otherwise = return $ DataBindingGroupDeclaration ds'

isTypeSynonym :: Declaration -> Maybe ProperName
isTypeSynonym (TypeSynonymDeclaration pn _ _) = Just pn
isTypeSynonym (PositionedDeclaration _ _ d) = isTypeSynonym d
isTypeSynonym _ = Nothing

fromValueDecl :: Declaration -> (Ident, NameKind, Expr)
fromValueDecl (ValueDeclaration ident nameKind [] (Right val)) = (ident, nameKind, val)
fromValueDecl ValueDeclaration{} = error "Binders should have been desugared"
fromValueDecl (PositionedDeclaration _ _ d) = fromValueDecl d
fromValueDecl _ = error "Expected ValueDeclaration"
