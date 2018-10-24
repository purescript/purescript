-- | This module implements the "Magic Do" optimization, which inlines calls to return
-- and bind for the Eff monad, as well as some of its actions.
module Language.PureScript.CoreImp.Optimizer.MagicDo (magicDoEffect, magicDoEff, magicDoST, inlineST) where

import Prelude.Compat
import Protolude (ordNub)

import Data.Maybe (fromJust, isJust)
import Data.Text (Text)

import Language.PureScript.CoreImp.AST
import Language.PureScript.CoreImp.Optimizer.Common
import Language.PureScript.PSString (mkString)
import qualified Language.PureScript.Constants as C

-- | Inline type class dictionaries for >>= and return for the Eff monad
--
-- E.g.
--
--  Prelude[">>="](dict)(m1)(function(x) {
--    return ...;
--  })
--
-- becomes
--
--  function __do {
--    var x = m1();
--    ...
--  }
magicDoEff :: AST -> AST
magicDoEff = magicDo C.eff C.effDictionaries

magicDoEffect :: AST -> AST
magicDoEffect = magicDo C.effect C.effectDictionaries

magicDoST :: AST -> AST
magicDoST = magicDo C.st C.stDictionaries

magicDo :: Text -> C.EffectDictionaries -> AST -> AST
magicDo effectModule C.EffectDictionaries{..} = everywhereTopDown convert
  where
  -- The name of the function block which is added to denote a do block
  fnName = "__do"
  -- Desugar monomorphic calls to >>= and return for the Eff monad
  convert :: AST -> AST
  -- Desugar pure
  convert (App _ (App _ pure' [val]) []) | isPure pure' = val
  -- Desugar discard
  convert (App _ (App _ bind [m]) [Function s1 Nothing [] (Block s2 js)]) | isDiscard bind =
    Function s1 (Just fnName) [] $ Block s2 (App s2 m [] : map applyReturns js )
  -- Desugar bind
  convert (App _ (App _ bind [m]) [Function s1 Nothing [arg] (Block s2 js)]) | isBind bind =
    Function s1 (Just fnName) [] $ Block s2 (VariableIntroduction s2 arg (Just (App s2 m [])) : map applyReturns js)
  -- Desugar untilE
  convert (App s1 (App _ f [arg]) []) | isEffFunc edUntil f =
    App s1 (Function s1 Nothing [] (Block s1 [ While s1 (Unary s1 Not (App s1 arg [])) (Block s1 []), Return s1 $ ObjectLiteral s1 []])) []
  -- Desugar whileE
  convert (App _ (App _ (App s1 f [arg1]) [arg2]) []) | isEffFunc edWhile f =
    App s1 (Function s1 Nothing [] (Block s1 [ While s1 (App s1 arg1 []) (Block s1 [ App s1 arg2 [] ]), Return s1 $ ObjectLiteral s1 []])) []
  -- Inline __do returns
  convert (Return _ (App _ (Function _ (Just ident) [] body) [])) | ident == fnName = body
  -- Inline double applications
  convert (App _ (App s1 (Function s2 Nothing [] (Block ss body)) []) []) =
    App s1 (Function s2 Nothing [] (Block ss (applyReturns `fmap` body))) []
  convert other = other
  -- Check if an expression represents a monomorphic call to >>= for the Eff monad
  isBind (App _ fn [dict]) | isDict (effectModule, edBindDict) dict && isBindPoly fn = True
  isBind _ = False
  -- Check if an expression represents a call to @discard@
  isDiscard (App _ (App _ fn [dict1]) [dict2])
    | isDict (C.controlBind, C.discardUnitDictionary) dict1 &&
      isDict (effectModule, edBindDict) dict2 &&
      isDiscardPoly fn = True
  isDiscard _ = False
  -- Check if an expression represents a monomorphic call to pure or return for the Eff applicative
  isPure (App _ fn [dict]) | isDict (effectModule, edApplicativeDict) dict && isPurePoly fn = True
  isPure _ = False
  -- Check if an expression represents the polymorphic >>= function
  isBindPoly = isDict (C.controlBind, C.bind)
  -- Check if an expression represents the polymorphic pure function
  isPurePoly = isDict (C.controlApplicative, C.pure')
  -- Check if an expression represents the polymorphic discard function
  isDiscardPoly = isDict (C.controlBind, C.discard)
  -- Check if an expression represents a function in the Effect module
  isEffFunc name (Indexer _ (StringLiteral _ name') (Var _ eff)) = eff == effectModule && name == name'
  isEffFunc _ _ = False

  applyReturns :: AST -> AST
  applyReturns (Return ss ret) = Return ss (App ss ret [])
  applyReturns (Block ss jss) = Block ss (map applyReturns jss)
  applyReturns (While ss cond js) = While ss cond (applyReturns js)
  applyReturns (For ss v lo hi js) = For ss v lo hi (applyReturns js)
  applyReturns (ForIn ss v xs js) = ForIn ss v xs (applyReturns js)
  applyReturns (IfElse ss cond t f) = IfElse ss cond (applyReturns t) (applyReturns `fmap` f)
  applyReturns other = other

-- | Inline functions in the ST module
inlineST :: AST -> AST
inlineST = everywhere convertBlock
  where
  -- Look for runST blocks and inline the STRefs there.
  -- If all STRefs are used in the scope of the same runST, only using { read, write, modify }STRef then
  -- we can be more aggressive about inlining, and actually turn STRefs into local variables.
  convertBlock (App s1 f [arg]) | isSTFunc C.runST f =
    let refs = ordNub . findSTRefsIn $ arg
        usages = findAllSTUsagesIn arg
        allUsagesAreLocalVars = all (\u -> let v = toVar u in isJust v && fromJust v `elem` refs) usages
        localVarsDoNotEscape = all (\r -> length (r `appearingIn` arg) == length (filter (\u -> let v = toVar u in v == Just r) usages)) refs
    in App s1 (everywhere (convert (allUsagesAreLocalVars && localVarsDoNotEscape)) arg) []
  convertBlock other = other
  -- Convert a block in a safe way, preserving object wrappers of references,
  -- or in a more aggressive way, turning wrappers into local variables depending on the
  -- agg(ressive) parameter.
  convert agg (App s1 f [arg]) | isSTFunc C.newSTRef f =
   Function s1 Nothing [] (Block s1 [Return s1 $ if agg then arg else ObjectLiteral s1 [(mkString C.stRefValue, arg)]])
  convert agg (App _ (App s1 f [ref]) []) | isSTFunc C.readSTRef f =
    if agg then ref else Indexer s1 (StringLiteral s1 C.stRefValue) ref
  convert agg (App _ (App _ (App s1 f [arg]) [ref]) []) | isSTFunc C.writeSTRef f =
    if agg then Assignment s1 ref arg else Assignment s1 (Indexer s1 (StringLiteral s1 C.stRefValue) ref) arg
  convert agg (App _ (App _ (App s1 f [func]) [ref]) []) | isSTFunc C.modifySTRef f =
    if agg then Assignment s1 ref (App s1 func [ref]) else Assignment s1 (Indexer s1 (StringLiteral s1 C.stRefValue) ref) (App s1 func [Indexer s1 (StringLiteral s1 C.stRefValue) ref])
  convert _ other = other
  -- Check if an expression represents a function in the ST module
  isSTFunc name (Indexer _ (StringLiteral _ name') (Var _ st)) = st == C.st && name == name'
  isSTFunc _ _ = False
  -- Find all ST Refs initialized in this block
  findSTRefsIn = everything (++) isSTRef
    where
    isSTRef (VariableIntroduction _ ident (Just (App _ (App _ f [_]) []))) | isSTFunc C.newSTRef f = [ident]
    isSTRef _ = []
  -- Find all STRefs used as arguments to readSTRef, writeSTRef, modifySTRef
  findAllSTUsagesIn = everything (++) isSTUsage
    where
    isSTUsage (App _ (App _ f [ref]) []) | isSTFunc C.readSTRef f = [ref]
    isSTUsage (App _ (App _ (App _ f [_]) [ref]) []) | isSTFunc C.writeSTRef f || isSTFunc C.modifySTRef f = [ref]
    isSTUsage _ = []
  -- Find all uses of a variable
  appearingIn ref = everything (++) isVar
    where
    isVar e@(Var _ v) | v == ref = [e]
    isVar _ = []
  -- Convert a AST value to a String if it is a Var
  toVar (Var _ v) = Just v
  toVar _ = Nothing
