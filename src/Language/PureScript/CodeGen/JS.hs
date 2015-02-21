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

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.CodeGen.JS (
    module AST,
    module Common,
    moduleToJs
) where

import Data.List ((\\), delete)
import Data.Maybe (mapMaybe)
import qualified Data.Traversable as T (traverse)

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad (foldM, replicateM, forM)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Supply.Class

import Language.PureScript.CodeGen.JS.AST as AST
import Language.PureScript.CodeGen.JS.Common as Common
import Language.PureScript.CoreFn
import Language.PureScript.Names
import Language.PureScript.CodeGen.JS.Optimizer
import Language.PureScript.Options
import Language.PureScript.Traversals (sndM)
import qualified Language.PureScript.Constants as C

-- |
-- Generate code in the simplified Javascript intermediate representation for all declarations in a
-- module.
--
moduleToJs :: (Functor m, Applicative m, Monad m, MonadReader (Options mode) m, MonadSupply m)
           => Module Ann -> m [JS]
moduleToJs (Module name imps exps foreigns decls) = do
  additional <- asks optionsAdditional
  jsImports <- T.traverse importToJs . delete (ModuleName [ProperName C.prim]) . (\\ [name]) $ imps
  let foreigns' = mapMaybe (\(_, js, _) -> js) foreigns
  jsDecls <- mapM (bindToJs name) decls
  optimized <- T.traverse (T.traverse optimize) jsDecls
  let isModuleEmpty = null exps
  let moduleBody = JSStringLiteral "use strict" : jsImports ++ foreigns' ++ concat optimized
  let exps' = JSObjectLiteral $ map (runIdent &&& JSVar . identToJs) exps
  return $ case additional of
    MakeOptions -> moduleBody ++ [JSAssignment (JSAccessor "exports" (JSVar "module")) exps']
    CompileOptions ns _ _ | not isModuleEmpty ->
      [ JSVariableIntroduction ns
                               (Just (JSBinary Or (JSVar ns) (JSObjectLiteral [])) )
      , JSAssignment (JSAccessor (moduleNameToJs name) (JSVar ns))
                     (JSApp (JSFunction Nothing [] (JSBlock (moduleBody ++ [JSReturn exps']))) [])
      ]
    _ -> []

-- |
-- Generates Javascript code for a module import.
--
importToJs :: (Monad m, MonadReader (Options mode) m) => ModuleName -> m JS
importToJs mn = do
  additional <- asks optionsAdditional
  let moduleBody = case additional of
        MakeOptions -> JSApp (JSVar "require") [JSStringLiteral (runModuleName mn)]
        CompileOptions ns _ _ -> JSAccessor (moduleNameToJs mn) (JSVar ns)
  return $ JSVariableIntroduction (moduleNameToJs mn) (Just moduleBody)

-- |
-- Generate code in the simplified Javascript intermediate representation for a declaration
--
bindToJs :: (Functor m, Applicative m, Monad m, MonadReader (Options mode) m, MonadSupply m)
         => ModuleName -> Bind Ann -> m [JS]
bindToJs mp (NonRec ident val) = return <$> nonRecToJS mp ident val
bindToJs mp (Rec vals) = forM vals (uncurry (nonRecToJS mp))

-- |
-- Generate code in the simplified Javascript intermediate representation for a single non-recursive
-- declaration.
--
-- The main purpose of this function is to handle code generation for comments.
--
nonRecToJS :: (Functor m, Applicative m, Monad m, MonadReader (Options mode) m, MonadSupply m)
           => ModuleName -> Ident -> Expr Ann -> m JS
nonRecToJS m i e@(extractAnn -> (_, com, _, _)) | not (null com) = do
  withoutComment <- asks optionsNoComments
  if withoutComment
     then nonRecToJS m i (modifyAnn removeComments e)
     else JSComment com <$> nonRecToJS m i (modifyAnn removeComments e)
nonRecToJS mp ident val = do
  js <- valueToJs mp val
  return $ JSVariableIntroduction (identToJs ident) (Just js)


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
valueToJs :: (Functor m, Applicative m, Monad m, MonadReader (Options mode) m, MonadSupply m)
          => ModuleName -> Expr Ann -> m JS
valueToJs m (Literal _ l) =
  literalToValueJS m l
valueToJs m (Var (_, _, _, Just (IsConstructor _ [])) name) =
  return $ JSAccessor "value" $ qualifiedToJS m id name
valueToJs m (Var (_, _, _, Just (IsConstructor _ _)) name) =
  return $ JSAccessor "create" $ qualifiedToJS m id name
valueToJs m (Accessor _ prop val) =
  accessorString prop <$> valueToJs m val
valueToJs m (ObjectUpdate _ o ps) = do
  obj <- valueToJs m o
  sts <- mapM (sndM $ valueToJs m) ps
  extendObj obj sts
valueToJs _ e@(Abs (_, _, _, Just IsTypeClassConstructor) _ _) =
  let args = unAbs e
  in return $ JSFunction Nothing (map identToJs args) (JSBlock $ map assign args)
  where
  unAbs :: Expr Ann -> [Ident]
  unAbs (Abs _ arg val) = arg : unAbs val
  unAbs _ = []
  assign :: Ident -> JS
  assign name = JSAssignment (accessorString (runIdent name) (JSVar "this"))
                             (var name)
valueToJs m (Abs _ arg val) = do
  ret <- valueToJs m val
  return $ JSFunction Nothing [identToJs arg] (JSBlock [JSReturn ret])
valueToJs m e@App{} = do
  let (f, args) = unApp e []
  args' <- mapM (valueToJs m) args
  case f of
    Var (_, _, _, Just IsNewtype) _ -> return (head args')
    Var (_, _, _, Just (IsConstructor _ fields)) name | length args == length fields ->
      return $ JSUnary JSNew $ JSApp (qualifiedToJS m id name) args'
    Var (_, _, _, Just IsTypeClassConstructor) name ->
      return $ JSUnary JSNew $ JSApp (qualifiedToJS m id name) args'
    _ -> flip (foldl (\fn a -> JSApp fn [a])) args' <$> valueToJs m f
  where
  unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
  unApp (App _ val arg) args = unApp val (arg : args)
  unApp other args = (other, args)
valueToJs m (Var _ ident) =
  return $ varToJs m ident
valueToJs m (Case _ values binders) = do
  vals <- mapM (valueToJs m) values
  bindersToJs m binders vals
valueToJs m (Let _ ds val) = do
  decls <- concat <$> mapM (bindToJs m) ds
  ret <- valueToJs m val
  return $ JSApp (JSFunction Nothing [] (JSBlock (decls ++ [JSReturn ret]))) []
valueToJs _ (Constructor (_, _, _, Just IsNewtype) _ (ProperName ctor) _) =
  return $ JSVariableIntroduction ctor (Just $
              JSObjectLiteral [("create",
                JSFunction Nothing ["value"]
                  (JSBlock [JSReturn $ JSVar "value"]))])
valueToJs _ (Constructor _ _ (ProperName ctor) []) =
  return $ iife ctor [ JSFunction (Just ctor) [] (JSBlock [])
         , JSAssignment (JSAccessor "value" (JSVar ctor))
              (JSUnary JSNew $ JSApp (JSVar ctor) []) ]
valueToJs _ (Constructor _ _ (ProperName ctor) fields) =
  let constructor =
        let body = [ JSAssignment (JSAccessor (identToJs f) (JSVar "this")) (var f) | f <- fields ]
        in JSFunction (Just ctor) (identToJs `map` fields) (JSBlock body)
      createFn =
        let body = JSUnary JSNew $ JSApp (JSVar ctor) (var `map` fields)
        in foldr (\f inner -> JSFunction Nothing [identToJs f] (JSBlock [JSReturn inner])) body fields
  in return $ iife ctor [ constructor
                        , JSAssignment (JSAccessor "create" (JSVar ctor)) createFn
                        ]
                        
iife :: String -> [JS] -> JS
iife v exprs = JSApp (JSFunction Nothing [] (JSBlock $ exprs ++ [JSReturn $ JSVar v])) []

literalToValueJS :: (Functor m, Applicative m, Monad m, MonadReader (Options mode) m, MonadSupply m)
                 => ModuleName -> Literal (Expr Ann) -> m JS
literalToValueJS _ (NumericLiteral n) = return $ JSNumericLiteral n
literalToValueJS _ (StringLiteral s) = return $ JSStringLiteral s
literalToValueJS _ (BooleanLiteral b) = return $ JSBooleanLiteral b
literalToValueJS m (ArrayLiteral xs) = JSArrayLiteral <$> mapM (valueToJs m) xs
literalToValueJS m (ObjectLiteral ps) = JSObjectLiteral <$> mapM (sndM (valueToJs m)) ps

-- |
-- Shallow copy an object.
--
extendObj :: (Applicative m, Monad m, MonadSupply m) => JS -> [(String, JS)] -> m JS
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
qualifiedToJS _ f (Qualified (Just (ModuleName [ProperName mn])) a) | mn == C.prim = JSVar . runIdent $ f a
qualifiedToJS m f (Qualified (Just m') a) | m /= m' = accessor (f a) (JSVar (moduleNameToJs m'))
qualifiedToJS _ f (Qualified _ a) = JSVar $ identToJs (f a)

-- |
-- Generate code in the simplified Javascript intermediate representation for pattern match binders
-- and guards.
--
bindersToJs :: forall m mode. (Functor m, Applicative m, Monad m, MonadReader (Options mode) m, MonadSupply m)
            => ModuleName -> [CaseAlternative Ann] -> [JS] -> m JS
bindersToJs m binders vals = do
  valNames <- replicateM (length vals) freshName
  let assignments = zipWith JSVariableIntroduction valNames (map Just vals)
  jss <- forM binders $ \(CaseAlternative bs result) -> do
    ret <- guardsToJs result
    go valNames ret bs
  return $ JSApp (JSFunction Nothing [] (JSBlock (assignments ++ concat jss ++ [JSThrow $ JSUnary JSNew $ JSApp (JSVar "Error") [JSStringLiteral "Failed pattern match"]])))
                 []
  where
    go :: [String] -> [JS] -> [Binder Ann] -> m [JS]
    go _ done [] = return done
    go (v:vs) done' (b:bs) = do
      done'' <- go vs done' bs
      binderToJs m v done'' b
    go _ _ _ = error "Invalid arguments to bindersToJs"

    guardsToJs :: Either [(Guard Ann, Expr Ann)] (Expr Ann) -> m [JS]
    guardsToJs (Left gs) = forM gs $ \(cond, val) -> do
      cond' <- valueToJs m cond
      done  <- valueToJs m val
      return $ JSIfElse cond' (JSBlock [JSReturn done]) Nothing
    guardsToJs (Right v) = return . JSReturn <$> valueToJs m v

-- |
-- Generate code in the simplified Javascript intermediate representation for a pattern match
-- binder.
--
binderToJs :: forall m mode. (Functor m, Applicative m, Monad m, MonadReader (Options mode) m, MonadSupply m)
           => ModuleName -> String -> [JS] -> Binder Ann -> m [JS]
binderToJs _ _ done (NullBinder{}) = return done
binderToJs m varName done (LiteralBinder _ l) =
  literalToBinderJS m varName done l
binderToJs _ varName done (VarBinder _ ident) =
  return (JSVariableIntroduction (identToJs ident) (Just (JSVar varName)) : done)
binderToJs m varName done (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [b]) =
  binderToJs m varName done b
binderToJs m varName done (ConstructorBinder (_, _, _, Just (IsConstructor ctorType fields)) _ ctor bs) = do
  js <- go (zip fields bs) done
  return $ case ctorType of
    ProductType -> js
    SumType ->
      [JSIfElse (JSInstanceOf (JSVar varName) (qualifiedToJS m (Ident . runProperName) ctor))
                (JSBlock js)
                Nothing]
  where
  go :: [(Ident, Binder Ann)] -> [JS] -> m [JS]
  go [] done' = return done'
  go ((field, binder) : remain) done' = do
    argVar <- freshName
    done'' <- go remain done'
    js <- binderToJs m argVar done'' binder
    return (JSVariableIntroduction argVar (Just (JSAccessor (identToJs field) (JSVar varName))) : js)
binderToJs m varName done binder@(ConstructorBinder _ _ ctor _) | isCons ctor = do
  let (headBinders, tailBinder) = uncons [] binder
      numberOfHeadBinders = fromIntegral $ length headBinders
  js1 <- foldM (\done' (headBinder, index) -> do
    headVar <- freshName
    jss <- binderToJs m headVar done' headBinder
    return (JSVariableIntroduction headVar (Just (JSIndexer (JSNumericLiteral (Left index)) (JSVar varName))) : jss)) done (zip headBinders [0..])
  tailVar <- freshName
  js2 <- binderToJs m tailVar js1 tailBinder
  return [JSIfElse (JSBinary GreaterThanOrEqualTo (JSAccessor "length" (JSVar varName)) (JSNumericLiteral (Left numberOfHeadBinders))) (JSBlock
    ( JSVariableIntroduction tailVar (Just (JSApp (JSAccessor "slice" (JSVar varName)) [JSNumericLiteral (Left numberOfHeadBinders)])) :
      js2
    )) Nothing]
  where
  uncons :: [Binder Ann] -> Binder Ann -> ([Binder Ann], Binder Ann)
  uncons acc (ConstructorBinder _ _ ctor' [h, t]) | isCons ctor' = uncons (h : acc) t
  uncons acc tailBinder = (reverse acc, tailBinder)
binderToJs _ _ _ b@(ConstructorBinder{}) =
  error $ "Invalid ConstructorBinder in binderToJs: " ++ show b
binderToJs m varName done (NamedBinder _ ident binder) = do
  js <- binderToJs m varName done binder
  return (JSVariableIntroduction (identToJs ident) (Just (JSVar varName)) : js)

literalToBinderJS :: forall m mode. (Functor m, Applicative m, Monad m, MonadReader (Options mode) m, MonadSupply m)
                  => ModuleName -> String -> [JS] -> Literal (Binder Ann) -> m [JS]
literalToBinderJS _ varName done (NumericLiteral num) =
  return [JSIfElse (JSBinary EqualTo (JSVar varName) (JSNumericLiteral num)) (JSBlock done) Nothing]
literalToBinderJS _ varName done (StringLiteral str) =
  return [JSIfElse (JSBinary EqualTo (JSVar varName) (JSStringLiteral str)) (JSBlock done) Nothing]
literalToBinderJS _ varName done (BooleanLiteral True) =
  return [JSIfElse (JSVar varName) (JSBlock done) Nothing]
literalToBinderJS _ varName done (BooleanLiteral False) =
  return [JSIfElse (JSUnary Not (JSVar varName)) (JSBlock done) Nothing]
literalToBinderJS m varName done (ObjectLiteral bs) = go done bs
  where
  go :: [JS] -> [(String, Binder Ann)] -> m [JS]
  go done' [] = return done'
  go done' ((prop, binder):bs') = do
    propVar <- freshName
    done'' <- go done' bs'
    js <- binderToJs m propVar done'' binder
    return (JSVariableIntroduction propVar (Just (accessorString prop (JSVar varName))) : js)
literalToBinderJS m varName done (ArrayLiteral bs) = do
  js <- go done 0 bs
  return [JSIfElse (JSBinary EqualTo (JSAccessor "length" (JSVar varName)) (JSNumericLiteral (Left (fromIntegral $ length bs)))) (JSBlock js) Nothing]
  where
  go :: [JS] -> Integer -> [Binder Ann] -> m [JS]
  go done' _ [] = return done'
  go done' index (binder:bs') = do
    elVar <- freshName
    done'' <- go done' (index + 1) bs'
    js <- binderToJs m elVar done'' binder
    return (JSVariableIntroduction elVar (Just (JSIndexer (JSNumericLiteral (Left index)) (JSVar varName))) : js)

isCons :: Qualified ProperName -> Bool
isCons (Qualified (Just mn) ctor) = mn == ModuleName [ProperName C.prim] && ctor == ProperName "Array"
isCons name = error $ "Unexpected argument in isCons: " ++ show name
