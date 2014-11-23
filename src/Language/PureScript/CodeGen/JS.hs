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

{-# LANGUAGE GADTs, DoAndIfThenElse #-}

module Language.PureScript.CodeGen.JS (
    module AST,
    declToJs,
    moduleToJs,
    identNeedsEscaping
) where

import Data.Maybe (catMaybes)
import Data.Function (on)
import Data.List (nub, (\\), delete, sortBy)

import qualified Data.Map as M

import Control.Monad (foldM, replicateM, forM)
import Control.Applicative
import Control.Arrow (second)

import Language.PureScript.Names
import Language.PureScript.AST
import Language.PureScript.Options
import Language.PureScript.CodeGen.JS.AST as AST
import Language.PureScript.Optimizer
import Language.PureScript.CodeGen.Common
import Language.PureScript.Environment
import Language.PureScript.Supply
import Language.PureScript.Traversals (sndM)
import qualified Language.PureScript.Constants as C

-- |
-- Generate code in the simplified Javascript intermediate representation for all declarations in a
-- module.
--
moduleToJs :: (Functor m, Applicative m, Monad m) => Options mode -> Module -> Environment -> SupplyT m [JS]
moduleToJs opts (Module name decls (Just exps)) env = do
  let jsImports = map (importToJs opts) . delete (ModuleName [ProperName C.prim]) . (\\ [name]) . nub $ concatMap imports decls
  jsDecls <- mapM (\decl -> declToJs opts name decl env) decls
  let optimized = concat $ map (map $ optimize opts) $ catMaybes jsDecls
  let isModuleEmpty = null exps
  let moduleBody = JSStringLiteral "use strict" : jsImports ++ optimized
  let moduleExports = JSObjectLiteral . map (second var) . M.toList . M.unions $ map exportToJs exps
  return $ case optionsAdditional opts of
    MakeOptions -> moduleBody ++ [JSAssignment (JSAccessor "exports" (JSVar "module")) moduleExports]
    CompileOptions ns _ _ | not isModuleEmpty ->
      [ JSVariableIntroduction ns
                               (Just (JSBinary Or (JSVar ns) (JSObjectLiteral [])) )
      , JSAssignment (JSAccessor (moduleNameToJs name) (JSVar ns))
                     (JSApp (JSFunction Nothing [] (JSBlock (moduleBody ++ [JSReturn moduleExports]))) [])
      ]
    _ -> []
moduleToJs _ _ _ = error "Exports should have been elaborated in name desugaring"

importToJs :: Options mode -> ModuleName -> JS
importToJs opts mn =
  JSVariableIntroduction (moduleNameToJs mn) (Just moduleBody)
  where
  moduleBody = case optionsAdditional opts of
    MakeOptions -> JSApp (JSVar "require") [JSStringLiteral (runModuleName mn)]
    CompileOptions ns _ _ -> JSAccessor (moduleNameToJs mn) (JSVar ns)

imports :: Declaration -> [ModuleName]
imports (ImportDeclaration mn _ _) = [mn]
imports other =
  let (f, _, _, _, _) = everythingOnValues (++) (const []) collectV collectB (const []) (const [])
  in f other
  where
  collectV :: Expr -> [ModuleName]
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
declToJs :: (Functor m, Applicative m, Monad m) => Options mode -> ModuleName -> Declaration -> Environment -> SupplyT m (Maybe [JS])
declToJs opts mp (ValueDeclaration ident _ _ (Right val)) e = do
  js <- valueToJs opts mp e val
  return $ Just [JSVariableIntroduction (identToJs ident) (Just js)]
declToJs opts mp (BindingGroupDeclaration vals) e = do
  jss <- forM vals $ \(ident, _, val) -> do
    js <- valueToJs opts mp e val
    return $ JSVariableIntroduction (identToJs ident) (Just js)
  return $ Just jss
declToJs _ _ (DataDeclaration Newtype _ _ [((ProperName ctor), _)]) _ =
  return $ Just $ [JSVariableIntroduction ctor (Just $
                    JSObjectLiteral [("create",
                      JSFunction Nothing ["value"]
                        (JSBlock [JSReturn $ JSVar "value"]))])]
declToJs _ _ (DataDeclaration Newtype _ _ _) _ =
  error "newtype has multiple constructors"
declToJs _ mp (DataDeclaration Data _ _ ctors) e = do
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
    go pn _ 0 values = JSUnary JSNew $ JSApp (JSVar $ runProperName pn) (reverse values)
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
exportToJs :: DeclarationRef -> M.Map String Ident
exportToJs (TypeRef _ (Just dctors)) = M.fromList [ (n, Ident n) | (ProperName n) <- dctors ]
exportToJs (ValueRef name)           = M.singleton (runIdent name) name
exportToJs (TypeInstanceRef name)    = M.singleton (runIdent name) name
exportToJs (TypeClassRef name)       = M.singleton (runProperName name) (Ident $ runProperName name)
exportToJs _                         = M.empty

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
valueToJs :: (Functor m, Applicative m, Monad m) => Options mode -> ModuleName -> Environment -> Expr -> SupplyT m JS
valueToJs _ _ _ (NumericLiteral n) = return $ JSNumericLiteral n
valueToJs _ _ _ (StringLiteral s) = return $ JSStringLiteral s
valueToJs _ _ _ (BooleanLiteral b) = return $ JSBooleanLiteral b
valueToJs opts m e (ArrayLiteral xs) = JSArrayLiteral <$> mapM (valueToJs opts m e) xs
valueToJs opts m e (ObjectLiteral ps) = JSObjectLiteral <$> mapM (sndM (valueToJs opts m e)) ps
valueToJs opts m e (TypeClassDictionaryConstructorApp name (TypedValue _ (ObjectLiteral ps) _)) =
  JSUnary JSNew . JSApp (qualifiedToJS m (Ident . runProperName) name) <$> mapM (valueToJs opts m e . snd) (sortBy (compare `on` fst) ps)
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
valueToJs opts m e v@App{} = do
  let (f, args) = unApp v []
  args' <- mapM (valueToJs opts m e) args
  case f of
    Constructor name | isNewtypeConstructor e name && length args == 1 -> return (head args')
    Constructor name | getConstructorArity e name == length args ->
      return $ JSUnary JSNew $ JSApp (qualifiedToJS m (Ident . runProperName) name) args'
    _ -> flip (foldl (\fn a -> JSApp fn [a])) args' <$> valueToJs opts m e f
  where
  unApp :: Expr -> [Expr] -> (Expr, [Expr])
  unApp (App val arg) args = unApp val (arg : args)
  unApp (PositionedValue _ val) args = unApp val args
  unApp (TypedValue _ val _) args = unApp val args
  unApp other args = (other, args)
valueToJs opts m e (Let ds val) = do
  decls <- concat . catMaybes <$> mapM (flip (declToJs opts m) e) ds
  ret <- valueToJs opts m e val
  return $ JSApp (JSFunction Nothing [] (JSBlock (decls ++ [JSReturn ret]))) []
valueToJs opts m e (Abs (Left arg) val) = do
  ret <- valueToJs opts m e val
  return $ JSFunction Nothing [identToJs arg] (JSBlock [JSReturn ret])
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
bindersToJs :: (Functor m, Applicative m, Monad m) => Options mode -> ModuleName -> Environment -> [CaseAlternative] -> [JS] -> SupplyT m JS
bindersToJs opts m e binders vals = do
  valNames <- replicateM (length vals) freshName
  let assignments = zipWith JSVariableIntroduction valNames (map Just vals)
  jss <- forM binders $ \(CaseAlternative bs result) -> do
    ret <- guardsToJs result
    go valNames ret bs
  return $ JSApp (JSFunction Nothing [] (JSBlock (assignments ++ concat jss ++ [JSThrow $ JSUnary JSNew $ JSApp (JSVar "Error") $ [JSStringLiteral "Failed pattern match"]])))
                 []
  where
    go :: (Functor m, Applicative m, Monad m) => [String] -> [JS] -> [Binder] -> SupplyT m [JS]
    go _ done [] = return done
    go (v:vs) done' (b:bs) = do
      done'' <- go vs done' bs
      binderToJs m e v done'' b
    go _ _ _ = error "Invalid arguments to bindersToJs"

    guardsToJs :: (Functor m, Applicative m, Monad m) => Either [(Guard, Expr)] Expr -> SupplyT m [JS]
    guardsToJs (Left gs) = forM gs $ \(cond, val) -> do
      cond' <- valueToJs opts m e cond
      done  <- valueToJs opts m e val
      return $ JSIfElse cond' (JSBlock [JSReturn done]) Nothing
    guardsToJs (Right v) = return . JSReturn <$> valueToJs opts m e v

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
binderToJs m e varName done (ConstructorBinder ctor bs) | isNewtypeConstructor e ctor =
  case bs of
    [b] -> binderToJs m e varName done b
    _ -> error "binder for newtype constructor should have a single argument"
binderToJs m e varName done (ConstructorBinder ctor bs) = do
  js <- go 0 done bs
  if isOnlyConstructor e ctor
  then
    return js
  else
    return [JSIfElse (JSBinary EqualTo (JSAccessor "$ctor" (JSVar varName)) (JSStringLiteral $ qnameToString ctor))
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
binderToJs m e varName done binder@(ConsBinder _ _) = do
  let (headBinders, tailBinder) = uncons [] binder
      numberOfHeadBinders = fromIntegral $ length headBinders
  js1 <- foldM (\done' (headBinder, index) -> do
    headVar <- freshName
    jss <- binderToJs m e headVar done' headBinder
    return (JSVariableIntroduction headVar (Just (JSIndexer (JSNumericLiteral (Left index)) (JSVar varName))) : jss)) done (zip headBinders [0..])
  tailVar <- freshName
  js2 <- binderToJs m e tailVar js1 tailBinder
  return [JSIfElse (JSBinary GreaterThanOrEqualTo (JSAccessor "length" (JSVar varName)) (JSNumericLiteral (Left numberOfHeadBinders))) (JSBlock
    ( JSVariableIntroduction tailVar (Just (JSApp (JSAccessor "slice" (JSVar varName)) [JSNumericLiteral (Left numberOfHeadBinders)])) :
      js2
    )) Nothing]
  where
  uncons :: [Binder] -> Binder -> ([Binder], Binder)
  uncons acc (ConsBinder h t) = uncons (h : acc) t
  uncons acc (PositionedBinder _ b) = uncons acc b
  uncons acc tailBinder = (reverse acc, tailBinder)
binderToJs m e varName done (NamedBinder ident binder) = do
  js <- binderToJs m e varName done binder
  return (JSVariableIntroduction (identToJs ident) (Just (JSVar varName)) : js)
binderToJs m e varName done (PositionedBinder _ binder) =
  binderToJs m e varName done binder

qnameToString :: Qualified ProperName -> String
qnameToString (Qualified (Just mn) pn) = moduleNameToJs mn ++ "." ++ runProperName pn
qnameToString _ = error "oops"
