-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.JS
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module generates code in the simplified Javascript intermediate representation from Purescript code
--
-----------------------------------------------------------------------------

{-# LANGUAGE DoAndIfThenElse #-}

module Language.PureScript.CodeGen.JS (
    module AST,
    ModuleType(..),
    declToJs,
    moduleToJs,
    identNeedsEscaping
) where

import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.Function (on)
import Data.List (nub, (\\), delete, sortBy)

import Control.Monad (replicateM, forM)
import Control.Applicative

import qualified Data.Map as M

import Language.PureScript.Names
import Language.PureScript.Declarations
import Language.PureScript.Options
import Language.PureScript.CodeGen.JS.AST as AST
import Language.PureScript.Types
import Language.PureScript.Optimizer
import Language.PureScript.CodeGen.Common
import Language.PureScript.Environment
import Language.PureScript.Supply
import Language.PureScript.Traversals (sndM)
import qualified Language.PureScript.Constants as C

-- |
-- Different types of modules which are supported
--
data ModuleType = CommonJS | Globals

-- |
-- Generate code in the simplified Javascript intermediate representation for all declarations in a
-- module.
--
moduleToJs :: (Functor m, Applicative m, Monad m) => ModuleType -> Options -> Module -> Environment -> SupplyT m [JS]
moduleToJs mt opts (Module name decls (Just exps)) env = do
  let jsImports = map (importToJs mt opts) . delete (ModuleName [ProperName C.prim]) . (\\ [name]) . nub $ concatMap imports decls
  jsDecls <- mapM (\decl -> declToJs opts name decl env) decls
  let optimized = concat $ map (map $ optimize opts) $ catMaybes jsDecls
  let isModuleEmpty = null optimized
  let moduleBody = JSStringLiteral "use strict" : jsImports ++ optimized
  let moduleExports = JSObjectLiteral $ concatMap exportToJs exps
  return $ case mt of
    CommonJS -> moduleBody ++ [JSAssignment (JSAccessor "exports" (JSVar "module")) moduleExports]
    Globals | not isModuleEmpty ->
      [ JSVariableIntroduction (fromJust (optionsBrowserNamespace opts))
                               (Just (JSBinary Or (JSVar (fromJust (optionsBrowserNamespace opts))) (JSObjectLiteral [])) )
      , JSAssignment (JSAccessor (moduleNameToJs name) (JSVar (fromJust (optionsBrowserNamespace opts))))
                     (JSApp (JSFunction Nothing [] (JSBlock (moduleBody ++ [JSReturn moduleExports]))) [])
      ]
    _ -> []
moduleToJs _ _ _ _ = error "Exports should have been elaborated in name desugaring"

importToJs :: ModuleType -> Options -> ModuleName -> JS
importToJs mt opts mn = JSVariableIntroduction (moduleNameToJs mn) (Just moduleBody)
  where
  moduleBody = case mt of
    CommonJS -> JSApp (JSVar "require") [JSStringLiteral (runModuleName mn)]
    Globals -> JSAccessor (moduleNameToJs mn) (JSVar (fromJust (optionsBrowserNamespace opts)))

imports :: Declaration -> [ModuleName]
imports (ImportDeclaration mn _ _) = [mn]
imports other =
  let (f, _, _, _, _) = everythingOnValues (++) (const []) collectV collectB (const []) (const [])
  in f other
  where
  collectV :: Value -> [ModuleName]
  collectV (Var (Qualified (Just mn) _)) = [mn]
  collectV (Constructor (Qualified (Just mn) _)) = [mn]
  collectV (TypeClassDictionaryConstructorApp (Qualified (Just mn) _) _) = [mn]
  collectV _ = []
  collectB :: Binder -> [ModuleName]
  collectB (ConstructorBinder (Qualified (Just mn) _) _) = [mn]
  collectB _ = []

-- |
-- Generate code in the simplified Javascript intermediate representation for a declaration
--
declToJs :: (Functor m, Applicative m, Monad m) => Options -> ModuleName -> Declaration -> Environment -> SupplyT m (Maybe [JS])
declToJs opts mp (ValueDeclaration ident _ _ _ val) e = do
  js <- valueToJs opts mp e val
  return $ Just [JSVariableIntroduction (identToJs ident) (Just js)]
declToJs opts mp (BindingGroupDeclaration vals) e = do
  jss <- flip mapM vals $ \(ident, _, val) -> do
    js <- valueToJs opts mp e val
    return $ JSVariableIntroduction (identToJs ident) (Just js)
  return $ Just jss
declToJs _ mp (DataDeclaration _ _ ctors) e = do
  return $ Just $ flip concatMap ctors $ \(pn@(ProperName ctor), tys) ->
      let propName = if isNullaryConstructor e (Qualified (Just mp) pn) then "value" else "create"
      in [ makeConstructor ctor (length tys)
         , JSAssignment (JSAccessor propName (JSVar ctor)) (go pn 0 (length tys) [])
         ]
    where
    makeConstructor :: String -> Int -> JS
    makeConstructor ctorName n =
      let
        args = [ "value" ++ show index | index <- [0..n-1] ]
        body = [ JSAssignment (JSAccessor arg (JSVar "this")) (JSVar arg) | arg <- args ]
      in JSFunction (Just ctorName) args (JSBlock body)
    go :: ProperName -> Int -> Int -> [JS] -> JS
    go pn _ 0 values = JSApp (JSNew (JSVar (runProperName pn))) (reverse values)
    go pn index n values =
      JSFunction Nothing ["value" ++ show index]
        (JSBlock [JSReturn (go pn (index + 1) (n - 1) (JSVar ("value" ++ show index) : values))])
declToJs opts mp (DataBindingGroupDeclaration ds) e = do
  jss <- mapM (\decl -> declToJs opts mp decl e) ds
  return $ Just $ concat $ catMaybes jss
declToJs _ _ (TypeClassDeclaration name _ supers members) _ =
  return $ Just $ [
    JSFunction (Just $ runProperName name) (identToJs `map` args)
      (JSBlock $ assn `map` args)]
  where
  assn :: Ident -> JS
  assn arg = JSAssignment (accessor arg (JSVar "this")) (var arg)
  args :: [Ident]
  args = sortBy (compare `on` runIdent) $ memberNames ++ superNames
  memberNames :: [Ident]
  memberNames = memberToName `map` members
  superNames :: [Ident]
  superNames = [ toSuperName superclass index
               | (index, (superclass, _)) <- zip [0..] supers
               ]
  toSuperName :: Qualified ProperName -> Integer -> Ident
  toSuperName pn index = Ident $ C.__superclass_ ++ show pn ++ "_" ++ show index
  memberToName :: Declaration -> Ident
  memberToName (TypeDeclaration ident _) = ident
  memberToName (PositionedDeclaration _ d) = memberToName d
  memberToName _ = error "Invalid declaration in type class definition"
declToJs _ _ (ExternDeclaration _ _ (Just js) _) _ = return $ Just [js]
declToJs opts mp (PositionedDeclaration _ d) e = declToJs opts mp d e
declToJs _ _ _ _ = return Nothing

-- |
-- Generate key//value pairs for an object literal exporting values from a module.
--
exportToJs :: DeclarationRef -> [(String, JS)]
exportToJs (TypeRef _ (Just dctors)) = map ((\n -> (n, var (Ident n))) . runProperName) dctors
exportToJs (ValueRef name) = [(runIdent name, var name)]
exportToJs (TypeInstanceRef name) = [(runIdent name, var name)]
exportToJs (TypeClassRef name) = [(runProperName name, var $ Ident $ runProperName name)]
exportToJs _ = []

-- |
-- Generate code in the simplified Javascript intermediate representation for a variable based on a
-- PureScript identifier.
--
var :: Ident -> JS
var = JSVar . identToJs

-- |
-- Generate code in the simplified Javascript intermediate representation for an accessor based on
-- a PureScript identifier. If the name is not valid in Javascript (symbol based, reserved name) an
-- indexer is returned.
--
accessor :: Ident -> JS -> JS
accessor (Ident prop) = accessorString prop
accessor (Op op) = JSIndexer (JSStringLiteral op)

accessorString :: String -> JS -> JS
accessorString prop | identNeedsEscaping prop = JSIndexer (JSStringLiteral prop)
                    | otherwise = JSAccessor prop

-- |
-- Generate code in the simplified Javascript intermediate representation for a value or expression.
--
valueToJs :: (Functor m, Applicative m, Monad m) => Options -> ModuleName -> Environment -> Value -> SupplyT m JS
valueToJs _ _ _ (NumericLiteral n) = return $ JSNumericLiteral n
valueToJs _ _ _ (StringLiteral s) = return $ JSStringLiteral s
valueToJs _ _ _ (BooleanLiteral b) = return $ JSBooleanLiteral b
valueToJs opts m e (ArrayLiteral xs) = JSArrayLiteral <$> mapM (valueToJs opts m e) xs
valueToJs opts m e (ObjectLiteral ps) = JSObjectLiteral <$> mapM (sndM (valueToJs opts m e)) ps
valueToJs opts m e (TypeClassDictionaryConstructorApp name (TypedValue _ (ObjectLiteral ps) _)) =
  JSApp (JSNew (qualifiedToJS m (Ident . runProperName) name)) <$> mapM (valueToJs opts m e . snd) (sortBy (compare `on` fst) ps)
valueToJs _ _ _ TypeClassDictionaryConstructorApp{} =
  error "TypeClassDictionaryConstructorApp did not contain object literal"
valueToJs opts m e (ObjectUpdate o ps) = do
  obj <- valueToJs opts m e o
  sts <- mapM (sndM (valueToJs opts m e)) ps
  extendObj obj sts
valueToJs _ m e (Constructor name) =
  let propName = if isNullaryConstructor e name then "value" else "create"
  in return $ JSAccessor propName $ qualifiedToJS m (Ident . runProperName) name
valueToJs opts m e (Case values binders) = do
  vals <- mapM (valueToJs opts m e) values
  bindersToJs opts m e binders vals
valueToJs opts m e (IfThenElse cond th el) = JSConditional <$> valueToJs opts m e cond <*> valueToJs opts m e th <*> valueToJs opts m e el
valueToJs opts m e (Accessor prop val) = accessorString prop <$> valueToJs opts m e val
valueToJs opts m e (App val arg) = JSApp <$> valueToJs opts m e val <*> (return <$> valueToJs opts m e arg)
valueToJs opts m e (Let ds val) = do
  decls <- concat . catMaybes <$> mapM (flip (declToJs opts m) e) ds
  ret <- valueToJs opts m e val
  return $ JSApp (JSFunction Nothing [] (JSBlock (decls ++ [JSReturn ret]))) []
valueToJs opts m e (Abs (Left arg) val) = do
  ret <- valueToJs opts m e val
  return $ JSFunction Nothing [identToJs arg] (JSBlock [JSReturn ret])
valueToJs opts m e (TypedValue _ (Abs (Left arg) val) ty) | optionsPerformRuntimeTypeChecks opts = do
  let arg' = identToJs arg
  ret <- valueToJs opts m e val
  return $ JSFunction Nothing [arg'] (JSBlock $ runtimeTypeChecks arg' ty ++ [JSReturn ret])
valueToJs _ m _ (Var ident) = return $ varToJs m ident
valueToJs opts m e (TypedValue _ val _) = valueToJs opts m e val
valueToJs opts m e (PositionedValue _ val) = valueToJs opts m e val
valueToJs _ _ _ (TypeClassDictionary _ _ _) = error "Type class dictionary was not replaced"
valueToJs _ _ _ _ = error "Invalid argument to valueToJs"

-- |
-- Shallow copy an object.
--
extendObj :: (Functor m, Applicative m, Monad m) => JS -> [(String, JS)] -> SupplyT m JS
extendObj obj sts = do
  newObj <- freshName
  key <- freshName
  let
    jsKey = JSVar key
    jsNewObj = JSVar newObj
    block = JSBlock (objAssign:copy:extend ++ [JSReturn jsNewObj])
    objAssign = JSVariableIntroduction newObj (Just $ JSObjectLiteral [])
    copy = JSForIn key obj $ JSBlock [JSIfElse cond assign Nothing]
    cond = JSApp (JSAccessor "hasOwnProperty" obj) [jsKey]
    assign = JSBlock [JSAssignment (JSIndexer jsKey jsNewObj) (JSIndexer jsKey obj)]
    stToAssign (s, js) = JSAssignment (JSAccessor s jsNewObj) js
    extend = map stToAssign sts
  return $ JSApp (JSFunction Nothing [] block) []

-- |
-- Generate code in the simplified Javascript intermediate representation for runtime type checks.
--
runtimeTypeChecks :: String -> Type -> [JS]
runtimeTypeChecks arg ty =
  let
    argTy = getFunctionArgumentType ty
  in
    maybe [] (argumentCheck (JSVar arg)) argTy
  where
  getFunctionArgumentType :: Type -> Maybe Type
  getFunctionArgumentType (TypeApp (TypeApp t funArg) _) | t == tyFunction = Just funArg
  getFunctionArgumentType (ForAll _ ty' _) = getFunctionArgumentType ty'
  getFunctionArgumentType _ = Nothing
  argumentCheck :: JS -> Type -> [JS]
  argumentCheck val t | t == tyNumber = [typeCheck val "number"]
  argumentCheck val t | t == tyString = [typeCheck val "string"]
  argumentCheck val t | t == tyBoolean = [typeCheck val "boolean"]
  argumentCheck val (TypeApp t _) | t == tyArray = [arrayCheck val]
  argumentCheck val (TypeApp t row) | t == tyObject =
    let
      (pairs, _) = rowToList row
    in
      typeCheck val "object" : concatMap (\(prop, ty') -> argumentCheck (accessorString prop val) ty') pairs
  argumentCheck val (TypeApp (TypeApp t _) _) | t == tyFunction = [typeCheck val "function"]
  argumentCheck val (ForAll _ ty' _) = argumentCheck val ty'
  argumentCheck _ _ = []
  typeCheck :: JS -> String -> JS
  typeCheck js ty' = JSIfElse (JSBinary NotEqualTo (JSTypeOf js) (JSStringLiteral ty')) (JSBlock [JSThrow (JSStringLiteral $ ty' ++ " expected")]) Nothing
  arrayCheck :: JS -> JS
  arrayCheck js = JSIfElse (JSUnary Not (JSApp (JSAccessor "isArray" (JSVar "Array")) [js])) (JSBlock [JSThrow (JSStringLiteral "Array expected")]) Nothing

-- |
-- Generate code in the simplified Javascript intermediate representation for a reference to a
-- variable.
--
varToJs :: ModuleName -> Qualified Ident -> JS
varToJs _ (Qualified Nothing ident) = var ident
varToJs m qual = qualifiedToJS m id qual

-- |
-- Generate code in the simplified Javascript intermediate representation for a reference to a
-- variable that may have a qualified name.
--
qualifiedToJS :: ModuleName -> (a -> Ident) -> Qualified a -> JS
qualifiedToJS m f (Qualified (Just m') a) | m /= m' = accessor (f a) (JSVar (moduleNameToJs m'))
qualifiedToJS _ f (Qualified _ a) = JSVar $ identToJs (f a)

-- |
-- Generate code in the simplified Javascript intermediate representation for pattern match binders
-- and guards.
--
bindersToJs :: (Functor m, Applicative m, Monad m) => Options -> ModuleName -> Environment -> [CaseAlternative] -> [JS] -> SupplyT m JS
bindersToJs opts m e binders vals = do
  valNames <- replicateM (length vals) freshName
  jss <- forM binders $ \(CaseAlternative bs grd result) -> do
    ret <- valueToJs opts m e result
    go valNames [JSReturn ret] bs grd
  return $ JSApp (JSFunction Nothing valNames (JSBlock (concat jss ++ [JSThrow (JSStringLiteral "Failed pattern match")])))
                 vals
  where
    go :: (Functor m, Applicative m, Monad m) => [String] -> [JS] -> [Binder] -> Maybe Guard -> SupplyT m [JS]
    go _ done [] Nothing = return done
    go _ done [] (Just cond) = do
      cond' <- valueToJs opts m e cond
      return [JSIfElse cond' (JSBlock done) Nothing]
    go (v:vs) done' (b:bs) grd = do
      done'' <- go vs done' bs grd
      binderToJs m e v done'' b
    go _ _ _ _ = error "Invalid arguments to bindersToJs"

-- |
-- Generate code in the simplified Javascript intermediate representation for a pattern match
-- binder.
--
binderToJs :: (Functor m, Applicative m, Monad m) => ModuleName -> Environment -> String -> [JS] -> Binder -> SupplyT m [JS]
binderToJs _ _ _ done NullBinder = return done
binderToJs _ _ varName done (StringBinder str) =
  return [JSIfElse (JSBinary EqualTo (JSVar varName) (JSStringLiteral str)) (JSBlock done) Nothing]
binderToJs _ _ varName done (NumberBinder num) =
  return [JSIfElse (JSBinary EqualTo (JSVar varName) (JSNumericLiteral num)) (JSBlock done) Nothing]
binderToJs _ _ varName done (BooleanBinder True) =
  return [JSIfElse (JSVar varName) (JSBlock done) Nothing]
binderToJs _ _ varName done (BooleanBinder False) =
  return [JSIfElse (JSUnary Not (JSVar varName)) (JSBlock done) Nothing]
binderToJs _ _ varName done (VarBinder ident) =
  return (JSVariableIntroduction (identToJs ident) (Just (JSVar varName)) : done)
binderToJs m e varName done (ConstructorBinder ctor bs) = do
  js <- go 0 done bs
  if isOnlyConstructor e ctor
  then
    return js
  else
    return [JSIfElse (JSInstanceOf (JSVar varName) (qualifiedToJS m (Ident . runProperName) ctor))
                     (JSBlock js)
                     Nothing]
  where
  go :: (Functor m, Applicative m, Monad m) => Integer -> [JS] -> [Binder] -> SupplyT m [JS]
  go _ done' [] = return done'
  go index done' (binder:bs') = do
    argVar <- freshName
    done'' <- go (index + 1) done' bs'
    js <- binderToJs m e argVar done'' binder
    return (JSVariableIntroduction argVar (Just (JSAccessor ("value" ++ show index) (JSVar varName))) : js)
binderToJs m e varName done (ObjectBinder bs) = go done bs
  where
  go :: (Functor m, Applicative m, Monad m) => [JS] -> [(String, Binder)] -> SupplyT m [JS]
  go done' [] = return done'
  go done' ((prop, binder):bs') = do
    propVar <- freshName
    done'' <- go done' bs'
    js <- binderToJs m e propVar done'' binder
    return (JSVariableIntroduction propVar (Just (accessorString prop (JSVar varName))) : js)
binderToJs m e varName done (ArrayBinder bs) = do
  js <- go done 0 bs
  return [JSIfElse (JSBinary EqualTo (JSAccessor "length" (JSVar varName)) (JSNumericLiteral (Left (fromIntegral $ length bs)))) (JSBlock js) Nothing]
  where
  go :: (Functor m, Applicative m, Monad m) => [JS] -> Integer -> [Binder] -> SupplyT m [JS]
  go done' _ [] = return done'
  go done' index (binder:bs') = do
    elVar <- freshName
    done'' <- go done' (index + 1) bs'
    js <- binderToJs m e elVar done'' binder
    return (JSVariableIntroduction elVar (Just (JSIndexer (JSNumericLiteral (Left index)) (JSVar varName))) : js)
binderToJs m e varName done (ConsBinder headBinder tailBinder) = do
  headVar <- freshName
  tailVar <- freshName
  js1 <- binderToJs m e headVar done headBinder
  js2 <- binderToJs m e tailVar js1 tailBinder
  return [JSIfElse (JSBinary GreaterThan (JSAccessor "length" (JSVar varName)) (JSNumericLiteral (Left 0))) (JSBlock
    ( JSVariableIntroduction headVar (Just (JSIndexer (JSNumericLiteral (Left 0)) (JSVar varName))) :
      JSVariableIntroduction tailVar (Just (JSApp (JSAccessor "slice" (JSVar varName)) [JSNumericLiteral (Left 1)])) :
      js2
    )) Nothing]
binderToJs m e varName done (NamedBinder ident binder) = do
  js <- binderToJs m e varName done binder
  return (JSVariableIntroduction (identToJs ident) (Just (JSVar varName)) : js)
binderToJs m e varName done (PositionedBinder _ binder) =
  binderToJs m e varName done binder

-- |
-- Checks whether a data constructor is the only constructor for that type, used to simplify the
-- check when generating code for binders.
--
isOnlyConstructor :: Environment -> Qualified ProperName -> Bool
isOnlyConstructor e ctor =
  let ty = fromMaybe (error "Data constructor not found") $ ctor `M.lookup` dataConstructors e
  in numConstructors (ctor, ty) == 1
  where
  numConstructors ty = length $ filter (((==) `on` typeConstructor) ty) $ M.toList $ dataConstructors e
  typeConstructor (Qualified (Just moduleName) _, (tyCtor, _)) = (moduleName, tyCtor)
  typeConstructor _ = error "Invalid argument to isOnlyConstructor"

isNullaryConstructor :: Environment -> Qualified ProperName -> Bool
isNullaryConstructor e ctor =
  not . isFunction . snd . fromMaybe (error "Data constructor not found") $ ctor `M.lookup` dataConstructors e
  where
  isFunction :: Type -> Bool
  isFunction (ForAll _ t _) = isFunction t
  isFunction (TypeApp t _) = isFunction t
  isFunction t = t == tyFunction
