-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.BundleOpt
-- Copyright   :  (c) Jürgen Nicklisch-Franken 2016
-- License     :  MIT
--
-- Maintainer  :  Jürgen Nicklisch <juergen.nicklisch@symbolian.net>
-- Stability   :  experimental
-- Portability :
--
-- | Optimize steps added to the bundle process for purescript.
--
-- This module takes as input the Javascript AST and applies optimizations like
-- uncurrying and inlining
-----------------------------------------------------------------------------

{-# LANGUAGE PatternGuards #-}

module Language.PureScript.BundleOpt (
    uncurryFunc
) where

import Debug.Trace
import Data.List (intersperse)
import qualified Data.Map as M

import Language.PureScript.BundleTypes
import Language.JavaScript.Parser.AST

-- | Main function for uncurry optimization
uncurryFunc :: [Module] -> [Module]
uncurryFunc modules =
    -- add uncurried functions
    let (modulesWithUncurried,adminMap) = foldr generateUncurried ([],M.empty) modules
    -- add exports
        (modulesWithExports,adminMap2)  = foldr generateUncurriedExports ([],adminMap) modulesWithUncurried

    in  trace (show adminMap2) $ modulesWithExports

-- | Data type with convenience functions

data FuncAdmin = FuncAdmin
    { moduleId :: ModuleIdentifier
    , arity :: Int
    , exported :: Bool}
    deriving (Eq,Show)

type FuncAdminMap = M.Map String [FuncAdmin]

addAdmin :: String -> FuncAdmin -> FuncAdminMap ->  FuncAdminMap
addAdmin funcName funcAdmin funcAdminMap = M.insertWith (++) funcName [funcAdmin] funcAdminMap

replaceAdmin :: String -> FuncAdmin -> FuncAdmin -> FuncAdminMap ->  FuncAdminMap
replaceAdmin funcName oldFuncAdmin newFuncAdmin funcAdminMap = M.adjust replaceFunc funcName funcAdminMap
    where
        replaceFunc [o] | o == oldFuncAdmin =  [newFuncAdmin]
        replaceFunc l = (filter (\e -> e /= oldFuncAdmin) l) ++  [newFuncAdmin]
        replaceFunc _ = error "BundleOpt>>replaceAdmin: Impossible with FuncAdmin"

findAdminFor :: String -> ModuleIdentifier -> FuncAdminMap -> Maybe FuncAdmin
findAdminFor name mid funcAdminMap =
    case M.lookup name funcAdminMap of
        Nothing -> Nothing
        Just li -> case filter (\fa -> moduleName (moduleId fa) == moduleName mid) li of
                        [] -> Nothing
                        [e] -> Just e
                        _ -> error "BundleOpt>>findAdminFor: Impossible with Export"

-- | Generation of uncurried functions
generateUncurried :: Module -> ([Module],FuncAdminMap) -> ([Module],FuncAdminMap)
generateUncurried (Module moduleIdentifier moduleElements) (modules, adminMap) =
        let (eles,adminMap') = foldr (generateUncurriedEle moduleIdentifier) ([],adminMap) moduleElements
        in (Module moduleIdentifier eles : modules, adminMap')

-- |  Generate uncurried functions from curried functions
generateUncurriedEle :: ModuleIdentifier -> ModuleElement -> ([ModuleElement],FuncAdminMap) -> ([ModuleElement],FuncAdminMap)
generateUncurriedEle mid m@(Member jSNode sort name [decl] keys) (eles, adminMap) -- a var decl
    | JSFunctionExpression fn _names lb [parameter] rb block <- node decl
    , JSLiteral "function" <- node fn
    , JSIdentifier idi <- node parameter
        = trace ("candidate: " ++ name ++ " para: " ++ show idi) $
            case analyzeUncurriedPrim [idi] block of
                Nothing -> (m : eles, adminMap)
                Just (argList,block) -> if not sort
                                            then trace ("generateFor: " ++ name) $ generateUncurried1 argList block m
                                            else trace ("generateFor2: " ++ name) $ generateUncurried2 argList block m
  where
    generateUncurried1 :: [String] -> JSNode -> ModuleElement -> ([ModuleElement],FuncAdminMap)
    generateUncurried1 argList block m@(Member jsNode typ name [decl] keys)
        | JSFunctionExpression fn names lb _ rb1 _ <- node decl
        , JSVariables var [ varIntro ] rb2 <- node jsNode
        , JSVarDecl declN (eq : decl) <- node varIntro
        = let newName = name ++ "_"
              newArgList    = intersperse (nt (JSLiteral ","))
                                    $ map (nt . JSIdentifier)
                                        argList
              newDecl       = NN $ JSFunctionExpression fn names lb newArgList rb1 block
              newVarIntro   = NN $ JSVarDecl (sp $ JSIdentifier newName) (eq : [newDecl])
              newNode       = NN $ JSVariables var [newVarIntro] rb2
              newAdmin      = FuncAdmin { moduleId = mid, arity = length argList, exported = False}
              in (m : Member newNode typ newName [newDecl] keys : eles, addAdmin name newAdmin adminMap)
    generateUncurried1 _ _ _ = (m : eles, adminMap)

    generateUncurried2 :: [String] -> JSNode -> ModuleElement -> ([ModuleElement],FuncAdminMap)
    generateUncurried2 argList block m@(Member jsNode typ name1 [decl] keys)
        | JSFunctionExpression fn names lb _ rb1 _ <- node decl
        , JSExpression (e : op : decl) <- node jsNode
        =   let newName = name ++ "_"
            in case setAccessor (node e) newName of
                Nothing -> (m : eles, adminMap)
                Just newE ->
                    let newArgList    = intersperse (nt (JSLiteral ","))
                                            $ map (nt . JSIdentifier)
                                                argList
                        newDecl       = NN $ JSFunctionExpression fn names lb newArgList rb1 block
                        newNode       = NN $ JSExpression (NN newE : op : [newDecl])
                        newAdmin      = FuncAdmin { moduleId = mid, arity = length argList, exported = False}
                    in (m : Member newNode typ newName [newDecl] keys : eles, addAdmin name newAdmin adminMap)
    generateUncurried2 _ _ _ = (m : eles, adminMap)
generateUncurriedEle _mid m (eles, adminMap) = (m : eles, adminMap)

analyzeUncurriedPrim :: [String] -> JSNode -> Maybe ([String], JSNode)
analyzeUncurriedPrim idList decl
    | JSBlock _ [ef] _      <- node decl
    , JSReturn _ [ef2] _    <- node ef
    , JSExpression [ef3]    <- node ef2
    , JSFunctionExpression fn _names lb [parameter] rb block <- node ef3
    , JSLiteral "function"  <- node fn
    , JSIdentifier idi      <- node parameter
    = trace ("found deeper: " ++ show idList ++ " para: " ++ show idi) $ analyzeUncurriedPrim (idi : idList) block
analyzeUncurriedPrim l@(a:b:_) block = Just (reverse l,block)
analyzeUncurriedPrim _ _ = Nothing

-- | Generation of uncurried exports
generateUncurriedExports :: Module -> ([Module],FuncAdminMap) -> ([Module],FuncAdminMap)
generateUncurriedExports (Module moduleIdentifier moduleElements) (modules, adminMap) =
        let (eles,adminMap') = foldr (generateUncurriedExpo moduleIdentifier) ([],adminMap) moduleElements
        in (Module moduleIdentifier eles : modules, adminMap')

generateUncurriedExpo :: ModuleIdentifier -> ModuleElement -> ([ModuleElement],FuncAdminMap) -> ([ModuleElement],FuncAdminMap)
generateUncurriedExpo mid (ExportsList l) (eles, adminMap) =
    let (newExports,adminMap') = foldr (generateUncurriedEx mid) ([],adminMap) l
    in (ExportsList newExports : eles, adminMap')
generateUncurriedExpo mid other (eles, adminMap) = (other : eles, adminMap)

generateUncurriedEx :: ModuleIdentifier -> (ExportType, String, JSNode, [Key])
    -> ([(ExportType, String, JSNode, [Key])],FuncAdminMap) -> ([(ExportType, String, JSNode, [Key])],FuncAdminMap)
generateUncurriedEx mid t@(RegularExport name1, name2, jSNode, [key]) (eles, adminMap)
    | JSIdentifier name3 <- node jSNode
    = -- [(ExportType, String, JSNode, [Key])]
        case findAdminFor name2 mid adminMap of
            Nothing -> (t : eles, adminMap)
            Just admin ->
                let newName   = name2 ++ "_"
                    newAdmin  = admin {exported = True}
                    newNode   = nt (JSIdentifier newName)
                    newKeys   = [(fst key,newName)]
                    newExport = (RegularExport (name1 ++ "_"), newName, newNode, newKeys)
                in  (t : newExport : eles, replaceAdmin name2 admin newAdmin adminMap)

generateUncurriedEx mid t@(ForeignReexport, name, jSNode, [key]) (eles, adminMap)
    | JSMemberDot [l] m r <- node jSNode
    , JSIdentifier "$foreign" <- node l
    , JSLiteral "." <- node m
    , JSIdentifier name2 <- node r
    =     case findAdminFor name2 mid adminMap of
            Nothing -> (t : eles, adminMap)
            Just admin ->
                let newName   = name ++ "_"
                    newAdmin  = admin {exported = True}
                    newNode   = NN (JSMemberDot [nt (JSIdentifier "$foreign")] (nt (JSLiteral "."))
                                        (nt (JSIdentifier newName)))
                    newKeys   = [(fst key,newName)]
                    newExport = (ForeignReexport, newName, newNode, newKeys)
                in  (t : newExport : eles, replaceAdmin name2 admin newAdmin adminMap)

generateUncurriedEx mid t (eles, adminMap) =
    trace ("export in unknown form: " ++ show t) (t : eles, adminMap)
