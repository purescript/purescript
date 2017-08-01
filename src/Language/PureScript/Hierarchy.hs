-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Hierarchy
-- Copyright   :  (c) Hardy Jones 2014
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Hardy Jones <jones3.hardy@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Generate Directed Graphs of PureScript TypeClasses
--
-----------------------------------------------------------------------------

module Language.PureScript.Hierarchy (typeClasses) where

import           Prelude.Compat
import           Protolude (ordNub)

import           Data.List (intercalate, sort)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Language.PureScript as P

newtype SuperMap = SuperMap
  { _unSuperMap :: Either (P.ProperName 'P.ClassName) (P.ProperName 'P.ClassName, P.ProperName 'P.ClassName)
  }
  deriving Eq

instance Ord SuperMap where
  compare (SuperMap s) (SuperMap s') = getCls s `compare` getCls s'
    where
      getCls = either id snd

prettyPrint :: SuperMap -> String
prettyPrint (SuperMap (Left sub)) =
  T.unpack (P.runProperName sub)
prettyPrint (SuperMap (Right (super, sub))) =
  T.unpack (P.runProperName super) <> " -> " <> T.unpack (P.runProperName sub)

runModuleName :: P.ModuleName -> String
runModuleName (P.ModuleName pns) = intercalate "_" ((T.unpack . P.runProperName) `map` pns)

typeClasses :: Functor f => f P.Module -> f (Maybe (String, String))
typeClasses ms =
  flip fmap ms $ \(P.Module _ _ moduleName decls _) ->
    let name = runModuleName moduleName
        tcs = filter P.isTypeClassDeclaration decls
        supers = sort . ordNub . filter (not . null) $ fmap superClasses tcs
        prologue = "digraph " ++ name ++ " {\n"
        body = intercalate "\n" (concatMap (fmap (\s -> "  " ++ prettyPrint s ++ ";")) supers)
        epilogue = "\n}"
        hier = prologue ++ body ++ epilogue
    in if null supers then Nothing else Just (name, hier)

superClasses :: P.Declaration -> [SuperMap]
superClasses (P.TypeClassDeclaration _ sub _ supers@(_:_) _ _) =
  fmap (\(P.Constraint (P.Qualified _ super) _ _) -> SuperMap (Right (super, sub))) supers
superClasses (P.TypeClassDeclaration _ sub _ _ _ _) = [SuperMap (Left sub)]
superClasses _ = []
