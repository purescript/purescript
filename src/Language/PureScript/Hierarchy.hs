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

module Language.PureScript.Hierarchy where

import           Prelude.Compat
import           Protolude (ordNub)

import           Data.List (sort)
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

data Graph = Graph
  { graphName :: GraphName
  , digraph :: Digraph
  }
  deriving (Eq, Show)

newtype GraphName = GraphName
  { _unGraphName :: T.Text
  }
  deriving (Eq, Show)

newtype Digraph = Digraph
  { _unDigraph :: T.Text
  }
  deriving (Eq, Show)

prettyPrint :: SuperMap -> T.Text
prettyPrint (SuperMap (Left sub)) = "  " <> P.runProperName sub <> ";"
prettyPrint (SuperMap (Right (super, sub))) =
  "  " <> P.runProperName super <> " -> " <> P.runProperName sub <> ";"

runModuleName :: P.ModuleName -> GraphName
runModuleName (P.ModuleName pns) =
  GraphName $ T.intercalate "_" (P.runProperName <$> pns)

typeClasses :: Functor f => f P.Module -> f (Maybe Graph)
typeClasses =
  fmap typeClassGraph

typeClassGraph :: P.Module -> Maybe Graph
typeClassGraph (P.Module _ _ moduleName decls _) =
  if null supers then Nothing else Just (Graph name graph)
    where
      name = runModuleName moduleName
      supers = sort . ordNub $ concatMap superClasses decls
      graph = Digraph $ typeClassPrologue name <> typeClassBody supers <> typeClassEpilogue

typeClassPrologue :: GraphName -> T.Text
typeClassPrologue (GraphName name) = "digraph " <> name <> " {\n"

typeClassBody :: [SuperMap] -> T.Text
typeClassBody supers = T.intercalate "\n" (prettyPrint <$> supers)

typeClassEpilogue :: T.Text
typeClassEpilogue = "\n}"

superClasses :: P.Declaration -> [SuperMap]
superClasses (P.TypeClassDeclaration _ sub _ supers@(_:_) _ _) =
  fmap (\(P.Constraint (P.Qualified _ super) _ _) -> SuperMap (Right (super, sub))) supers
superClasses (P.TypeClassDeclaration _ sub _ _ _ _) = [SuperMap (Left sub)]
superClasses _ = []
